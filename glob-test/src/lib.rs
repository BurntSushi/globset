/*!
A crate for running glob tests.

The purpose of this crate is to make it convenient to manage a corpus of glob
tests, and execute them against a glob implementation. In particular, glob
tests are written in a TOML file, where each test encodes the inputs and
expected outputs of each glob.

This crate will read and load such files into memory, while also providing
a runner for executing the loaded glob tests.

The `GLOB_TEST` enviornment variable can be used to skip some tests via a
whitelist and/or blacklist mechanism. For example,

```ignore
GLOB_TEST=foo,-bar
```

will run tests containing `foo`, but not `bar`. For more details on
`GLOB_TEST`, see [`TestRunner`](struct.TestRunner.html).
*/

#![deny(missing_docs)]

pub extern crate bstr;

use std::borrow::Borrow;
use std::collections::HashSet;
use std::env;
use std::error;
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use bstr::{BStr, BString, ByteSlice, ByteVec};
use regex::{self, Regex};
use serde::{self, Deserialize};
use snafu::{OptionExt, ResultExt, Snafu};

// mod escape;

const ENV_GLOB_TEST: &str = "GLOB_TEST";

/// An error that may occur in the course of loading or running glob tests.
#[derive(Debug, Snafu)]
pub struct Error(ErrorKind);

#[derive(Snafu)]
enum ErrorKind {
    #[snafu(display("{}: {}", path.display(), source))]
    FileReadError { path: PathBuf, source: io::Error },
    #[snafu(display("invalid TOML: {}", source))]
    TomlReadError { source: toml::de::Error },
    #[snafu(display("{}: failed to load test data: {}", path.display(), source))]
    ReadError {
        path: PathBuf,
        #[snafu(source(from(Error, Box::new)))]
        source: Box<Error>,
    },
    #[snafu(display("{}: no file name in path", path.display()))]
    NoFileName { path: PathBuf },
    #[snafu(display("{}: file name contains invalid UTF-8", path.display()))]
    FileNameInvalidUTF8 { path: PathBuf },
    #[snafu(display("error reading env var {}: {:?}", var, value))]
    EnvVarInvalidUTF8 { var: String, value: BString },
    #[snafu(display("error for include pattern '{}': {}", pattern, source))]
    IncludePatternError { pattern: String, source: regex::Error },
    #[snafu(display("no globs given for test '{}'", test_name))]
    NoGlobs { test_name: String },
    #[snafu(display("duplicate test named '{}'", test_name))]
    DuplicateTests { test_name: String },
}

// We implement fmt::Debug using ErrorKind's fmt::Display implementation
// because this is what is show to users via Rust's unit testing framework.
impl fmt::Debug for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// A collection of glob tests.
#[derive(Clone, Debug, Deserialize)]
pub struct GlobTests {
    tests: Vec<GlobTest>,
    #[serde(skip)]
    seen: HashSet<String>,
}

/// A glob test describes the inputs and expected outputs of a glob match.
#[derive(Clone, Debug, Deserialize)]
pub struct GlobTest {
    #[serde(skip)]
    group: String,
    #[serde(default)]
    name: String,
    #[serde(skip)]
    full_name: String,
    glob: Globs,
    input: BString,
    matches: GlobMatches,
    #[serde(default)]
    error_compile: bool,
    #[serde(default)]
    case_insensitive: bool,
    #[serde(default)]
    literal_separator: bool,
    #[serde(default)]
    backslash_escape: bool,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
enum Globs {
    Many(Vec<BString>),
    One(BString),
}

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
enum GlobMatches {
    YesNo(bool),
    Which(Vec<usize>),
}

impl GlobTests {
    /// Create a new empty collection of glob tests.
    pub fn new() -> GlobTests {
        GlobTests { tests: vec![], seen: HashSet::new() }
    }

    /// Loads all of the tests in the given TOML file. The group name assigned
    /// to each test is the stem of the file name. For example, if one loads
    /// `foo/bar.toml`, then the group name for each test will be `bar`.
    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        let path = path.as_ref();
        let data = fs::read(path)
            .with_context(|| FileReadError { path: path.to_path_buf() })?;
        let group_name = path
            .file_stem()
            .with_context(|| NoFileName { path: path.to_path_buf() })?
            .to_str()
            .with_context(|| FileNameInvalidUTF8 {
                path: path.to_path_buf(),
            })?;
        self.load_slice(&group_name, &data)
            .with_context(|| ReadError { path: path.to_path_buf() })?;
        Ok(())
    }

    /// Load all of the TOML encoded tests in `data` into this collection.
    /// The given group name is assigned to all loaded tests.
    pub fn load_slice(
        &mut self,
        group_name: &str,
        data: &[u8],
    ) -> Result<(), Error> {
        let mut index = 1;
        let mut tests: GlobTests =
            toml::from_slice(&data).context(TomlReadError)?;
        for t in &mut tests.tests {
            t.group = group_name.to_string();
            if t.name.is_empty() {
                t.name = format!("{}", index);
                index += 1;
            }
            t.full_name = format!("{}/{}", t.group, t.name);
            if t.globs().is_empty() {
                return Err(Error(ErrorKind::NoGlobs {
                    test_name: t.full_name().to_string(),
                }));
            }
            if self.seen.contains(t.full_name()) {
                return Err(Error(ErrorKind::DuplicateTests {
                    test_name: t.full_name().to_string(),
                }));
            }
            self.seen.insert(t.full_name().to_string());
        }
        self.tests.extend(tests.tests);
        Ok(())
    }

    /// Return an iterator over all glob tests that have been loaded. The
    /// order of the iterator corresponds to the order in which the tests were
    /// loaded.
    pub fn iter(&self) -> GlobTestsIter {
        GlobTestsIter { it: self.tests.iter() }
    }
}

/// An iterator of `GlobTest`s.
#[derive(Debug)]
pub struct GlobTestsIter<'a> {
    it: std::slice::Iter<'a, GlobTest>,
}

impl<'a> Iterator for GlobTestsIter<'a> {
    type Item = &'a GlobTest;

    fn next(&mut self) -> Option<&'a GlobTest> {
        self.it.next()
    }
}

impl GlobTest {
    fn test(&self, glob: &CompiledGlob) -> TestResult {
        match glob.match_glob {
            None => TestResult::skip(),
            Some(ref match_glob) => match_glob(self),
        }
    }

    /// Return the group name of this test.
    ///
    /// Usually the group name corresponds to a collection of related tests.
    pub fn group(&self) -> &str {
        &self.group
    }

    /// The name of this test.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The full name of this test, which is formed by joining the group
    /// name with the test name via a `/`.
    pub fn full_name(&self) -> &str {
        &self.full_name
    }

    /// Return all of the globs that should be matched for this test. This
    /// slice is guaranteed to be non-empty.
    pub fn globs(&self) -> &[BString] {
        match self.glob {
            Globs::One(ref glob) => std::slice::from_ref(glob),
            Globs::Many(ref globs) => globs,
        }
    }

    /// Return the text on which the glob should be matched.
    pub fn input(&self) -> &BStr {
        self.input.as_bstr()
    }

    /// Returns true if and only if this test expects at least one of the globs
    /// to match the input.
    pub fn is_match(&self) -> bool {
        match self.matches {
            GlobMatches::YesNo(yesno) => yesno,
            GlobMatches::Which(ref which) => !which.is_empty(),
        }
    }

    /// Returns a slice of globs that are expected to match the input. The
    /// slice is empty if no match is expected to occur. The indices returned
    /// here correspond to the indices of the slice returned by the `globs`
    /// method.
    pub fn which_matches(&self) -> &[usize] {
        match self.matches {
            GlobMatches::YesNo(yesno) => {
                if yesno {
                    &[0]
                } else {
                    &[]
                }
            }
            GlobMatches::Which(ref which) => which,
        }
    }

    /// Returns true if the given glob is expected to not compile.
    pub fn error_compile(&self) -> bool {
        self.error_compile
    }

    /// Returns true if glob matching should be performed without regard to
    /// case.
    pub fn case_insensitive(&self) -> bool {
        self.case_insensitive
    }

    /// Returns true if `/` should only be matched when there is a literal
    /// `/` in the glob pattern. That is, neither `*` nor `?` should match
    /// a `/`.
    pub fn literal_separator(&self) -> bool {
        self.literal_separator
    }

    /// Returns true if `\\` can be used to escape meta characters in a glob.
    /// When this is disabled, `\\` is treated as a literal backslash.
    pub fn backslash_escape(&self) -> bool {
        self.backslash_escape
    }
}

/// The result of compiling a glob.
///
/// In many implementations, the act of matching a glob can be separated from
/// the act of compiling a glob. A `CompiledGlob` represents a glob that has
/// been compiled and is ready to be used for matching.
pub struct CompiledGlob {
    match_glob: Option<Box<dyn Fn(&GlobTest) -> TestResult>>,
}

impl CompiledGlob {
    /// Provide a closure that represents the compiled glob and executes a
    /// glob match on any `GlobTest`. The `GlobTest` given to the closure
    /// provided is the exact same `GlobTest` that is used to compile this
    /// glob.
    pub fn compiled<F: Fn(&GlobTest) -> TestResult + 'static>(
        match_glob: F,
    ) -> CompiledGlob {
        CompiledGlob { match_glob: Some(Box::new(match_glob)) }
    }

    /// Indicate that tests on this glob should be skipped. This typically
    /// occurs if the `GlobTest` requires something that an implementation
    /// does not support.
    pub fn skip() -> CompiledGlob {
        CompiledGlob { match_glob: None }
    }
}

impl fmt::Debug for CompiledGlob {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let status = match self.match_glob {
            None => "Skip",
            Some(_) => "Run(...)",
        };
        f.debug_struct("CompiledGlob").field("match_glob", &status).finish()
    }
}

/// The result of executing a single glob match.
///
/// When using the test runner, callers must provide a closure that takes
/// a `GlobTest` and returns a `TestResult`. The `TestResult` is meant to
/// capture the results of matching the input against the glob specified by
/// the `GlobTest`.
#[derive(Debug, Clone)]
pub struct TestResult(TestResultInner);

#[derive(Debug, Clone)]
enum TestResultInner {
    Matched { which: Vec<usize> },
    Skip,
}

impl TestResult {
    /// Create a test result that indicates a match.
    pub fn matched() -> TestResult {
        TestResult::which(vec![0])
    }

    /// Create a test result that indicates the glob did not match.
    pub fn no_match() -> TestResult {
        TestResult::which(vec![])
    }

    /// Create a test result that indicates which out of possibly many globs
    /// matched the input. If `which` is empty, then this is equivalent to
    /// `TestResult::no_match()`.
    pub fn which(which: Vec<usize>) -> TestResult {
        TestResult(TestResultInner::Matched { which })
    }

    /// Indicate that this test should be skipped. It will not be counted as
    /// a failure.
    pub fn skip() -> TestResult {
        TestResult(TestResultInner::Skip)
    }
}

/// A runner for executing glob tests.
///
/// This runner is intended to be used within a Rust unit test, marked with the
/// `#[test]` attribute.
///
/// A test runner is responsible for running tests against a glob
/// implementation. It contains logic for skipping tests and collects test
/// results. Typical usage corresponds to calling `test_all` on an iterator
/// of `GlobTest`s, and then calling `assert` once done. If any tests failed,
/// then `assert` will panic with an error message containing all test
/// failures. `assert` must be called before the test completes.
///
/// ### Skipping tests
///
/// If the `GLOB_TEST` environment variable is set, then it may contain a comma
/// separated list of regexes. Each regex corresponds to a whitelisted pattern,
/// unless it starts with a `-`, in which case it corresponds to a blacklisted
/// pattern.
///
/// If there are any whitelist patterns, then a test's full name must match
/// at least one of the whitelist patterns in order to be run. If there are
/// no whitelist patterns, then a test is run only when it does not match any
/// blacklist patterns.
///
/// The last pattern that a test name matches takes precedent.
///
/// Callers may also specify explicit whitelist or blacklist rules using the
/// corresponding methods on this type.
///
/// Whitelist and blacklist patterns are matched on the full name of each
/// test, which typically looks like `base_file_stem/test_name`.
#[derive(Debug)]
pub struct TestRunner {
    include: Vec<IncludePattern>,
    results: GlobTestResults,
}

#[derive(Debug)]
struct IncludePattern {
    blacklist: bool,
    re: Regex,
}

impl TestRunner {
    /// Create a new runner for executing tests.
    ///
    /// The test runner maintains a full list of tests that have succeeded,
    /// failed or been skipped. Moreover, the test runner may control which
    /// tests get run via its whitelist and blacklist.
    ///
    /// If the `GLOB_TEST` environment variable is set, then it may contain
    /// a comma separated list of regexes. Each regex corresponds to a
    /// whitelisted pattern, unless it starts with a `-`, in which case it
    /// corresponds to a blacklisted pattern.
    ///
    /// If there are any whitelist patterns, then a test's full name must
    /// match at least one of the whitelist patterns in order to be run. If
    /// there are no whitelist patterns, then a test is run only when it does
    /// not match any blacklist patterns.
    ///
    /// The last pattern that a test name matches takes precedent.
    ///
    /// If there was a problem compiling any of the patterns or reading the
    /// environment variable, then an error is returned.
    pub fn new() -> Result<TestRunner, Error> {
        let mut runner =
            TestRunner { include: vec![], results: GlobTestResults::new() };
        for mut pattern in read_env(ENV_GLOB_TEST)?.split(",") {
            pattern = pattern.trim();
            if pattern.is_empty() {
                continue;
            }
            if pattern.starts_with("-") {
                runner.blacklist(&pattern[1..])?;
            } else {
                runner.whitelist(pattern)?;
            }
        }
        Ok(runner)
    }

    /// Assert that all tests run have either passed or have been skipped.
    ///
    /// If any tests have failed, then a panic occurs with a report of all
    /// failures.
    pub fn assert(&mut self) {
        self.results.assert();
    }

    /// Like assert, but prints out all tests and their status, followed by
    /// any failures.
    pub fn assert_verbose(&mut self) {
        self.results.assert_verbose();
    }

    /// Whitelist the given regex pattern. If the given pattern is invalid,
    /// then an error is returned.
    pub fn whitelist(&mut self, pattern: &str) -> Result<(), Error> {
        let re = Regex::new(pattern).with_context(|| IncludePatternError {
            pattern: pattern.to_string(),
        })?;
        self.include.push(IncludePattern { blacklist: false, re });
        Ok(())
    }

    /// Blacklist the given regex pattern. If the given pattern is invalid,
    /// then an error is returned.
    ///
    /// A blacklisted test is never run, unless a whitelist pattern added after
    /// the blacklist pattern matches it.
    pub fn blacklist(&mut self, pattern: &str) -> Result<(), Error> {
        let re = Regex::new(pattern).with_context(|| IncludePatternError {
            pattern: pattern.to_string(),
        })?;
        self.include.push(IncludePattern { blacklist: true, re });
        Ok(())
    }

    /// Run all of the given tests.
    pub fn test_iter<I, T>(
        &mut self,
        it: I,
        compile: impl Fn(
            &GlobTest,
            &[BString],
        ) -> Result<CompiledGlob, Box<dyn error::Error>>,
    ) -> &mut TestRunner
    where
        I: IntoIterator<Item = T>,
        T: Borrow<GlobTest>,
    {
        for test in it {
            let test = test.borrow();
            if self.should_skip(test) {
                self.results.skip(test);
                continue;
            }
            self.test(test, |globs| compile(test, globs));
        }
        self
    }

    /// Run a single test.
    ///
    /// This records the result of running the test in this runner. This does
    /// not fail the test immediately if the given glob test fails. Instead,
    /// this is only done when the `assert` method is called.
    ///
    /// Note that using this method bypasses any include patterns applied to
    /// this runner. Include patterns are only applied when using `test_iter`.
    pub fn test(
        &mut self,
        test: &GlobTest,
        compile: impl Fn(&[BString]) -> Result<CompiledGlob, Box<dyn error::Error>>,
    ) -> &mut TestRunner {
        let compiled = match compile(test.globs()) {
            Ok(compiled) => compiled,
            Err(err) => {
                if test.error_compile() {
                    self.results.pass(test);
                } else {
                    self.results
                        .fail(test, GlobTestFailureKind::CompileError { err });
                }
                return self;
            }
        };
        if test.error_compile() {
            self.results.fail(test, GlobTestFailureKind::NoCompileError);
            return self;
        }
        match test.test(&compiled).0 {
            TestResultInner::Skip => {
                self.results.skip(test);
            }
            TestResultInner::Matched { which } => {
                if which.is_empty() && test.is_match() {
                    self.results.fail(test, GlobTestFailureKind::IsMatch);
                } else if !which.is_empty() && !test.is_match() {
                    self.results.fail(test, GlobTestFailureKind::IsMatch);
                } else if which != test.which_matches() {
                    self.results
                        .fail(test, GlobTestFailureKind::Many { got: which });
                } else {
                    self.results.pass(test);
                }
            }
        }
        self
    }

    /// Return true if and only if the given test should be skipped.
    fn should_skip(&self, test: &GlobTest) -> bool {
        if self.include.is_empty() {
            return false;
        }

        // If we don't have any whitelist patterns, then the test will be run
        // unless it is blacklisted. Otherwise, if there are whitelist
        // patterns, then the test must match at least one of them.
        let mut skip = self.include.iter().any(|pat| !pat.blacklist);
        for pat in &self.include {
            if pat.re.is_match(test.full_name()) {
                skip = pat.blacklist;
            }
        }
        skip
    }
}

/// A collection of test results, corresponding to passed, skipped and failed
/// tests.
#[derive(Debug)]
struct GlobTestResults {
    pass: Vec<GlobTest>,
    fail: Vec<GlobTestFailure>,
    skip: Vec<GlobTest>,
}

/// A test that failed along with the reason why.
#[derive(Debug)]
struct GlobTestFailure {
    test: GlobTest,
    kind: GlobTestFailureKind,
}

/// Describes the nature of the failed test.
#[derive(Debug)]
enum GlobTestFailureKind {
    /// This occurs when the test expected a match (or didn't expect a match),
    /// but the actual glob implementation didn't match (or did match).
    IsMatch,
    /// This occurs when a set of globs is tested, and the matching globs
    /// returned by the glob implementation don't match the expected matching
    /// globs. This error contains the indices of the globs that matched.
    Many { got: Vec<usize> },
    /// This occurs when the test expected the glob to fail to compile, but it
    /// compiled successfully.
    NoCompileError,
    /// This occurs when the test expected the glob to compile successfully,
    /// but it failed to compile.
    CompileError { err: Box<dyn error::Error> },
}

impl GlobTestResults {
    fn new() -> GlobTestResults {
        GlobTestResults { pass: vec![], fail: vec![], skip: vec![] }
    }

    fn pass(&mut self, test: &GlobTest) {
        self.pass.push(test.clone());
    }

    fn fail(&mut self, test: &GlobTest, kind: GlobTestFailureKind) {
        self.fail.push(GlobTestFailure { test: test.clone(), kind });
    }

    fn skip(&mut self, test: &GlobTest) {
        self.skip.push(test.clone());
    }

    fn assert(&self) {
        if self.fail.is_empty() {
            return;
        }
        let failures = self
            .fail
            .iter()
            .map(|f| f.to_string())
            .collect::<Vec<String>>()
            .join("\n\n");
        panic!(
            "found {} failures:\n{}\n{}\n{}\n\n\
             Set the GLOB_TEST environment variable to filter tests, \n\
             e.g., GLOB_TEST=foo,-foo2 runs every test whose name contains \n\
             foo but not foo2\n\n",
            self.fail.len(),
            "~".repeat(79),
            failures.trim(),
            "~".repeat(79),
        )
    }

    fn assert_verbose(&self) {
        println!("{}", "~".repeat(79));
        for t in &self.pass {
            println!("pass: {}", t.full_name());
        }
        for t in &self.skip {
            println!("skip: {}", t.full_name());
        }
        for t in &self.fail {
            println!("FAIL: {}", t.test.full_name());
        }
        println!(
            "\npassed: {}, skipped: {}, failed: {}",
            self.pass.len(),
            self.skip.len(),
            self.fail.len()
        );
        println!("{}", "~".repeat(79));

        self.assert();
    }
}

impl fmt::Display for GlobTestFailure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: {}\n    \
             pattern: {:?}\n    \
             input:   {}",
            self.test.full_name(),
            self.kind.fmt(&self.test)?,
            self.test.globs(),
            self.test.input(),
        )
    }
}

impl GlobTestFailureKind {
    fn fmt(&self, test: &GlobTest) -> Result<String, fmt::Error> {
        use std::fmt::Write;

        let mut buf = String::new();
        match *self {
            GlobTestFailureKind::IsMatch => {
                if test.is_match() {
                    write!(buf, "expected match, but none found")?;
                } else {
                    write!(buf, "expected no match, but found a match")?;
                }
            }
            GlobTestFailureKind::Many { ref got } => {
                write!(
                    buf,
                    "expected globs {:?} to match,  but found {:?}",
                    test.which_matches(),
                    got
                )?;
            }
            GlobTestFailureKind::NoCompileError => {
                write!(buf, "expected glob to NOT compile, but it did")?;
            }
            GlobTestFailureKind::CompileError { ref err } => {
                write!(buf, "expected glob to compile, failed: {}", err)?;
            }
        }
        Ok(buf)
    }
}

/// Read the environment variable given. If it doesn't exist, then return an
/// empty string. Otherwise, check that it is valid UTF-8. If it isn't, return
/// a useful error message.
fn read_env(var: &str) -> Result<String, Error> {
    let val = match env::var_os(var) {
        None => return Ok("".to_string()),
        Some(val) => val,
    };
    val.into_string().map_err(|os| {
        let value = BString::from(Vec::from_os_str_lossy(&os).into_owned());
        Error(ErrorKind::EnvVarInvalidUTF8 { var: var.to_string(), value })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load1() {
        let data = r#"
[[tests]]
name = "foo"
glob = "*.rs"
input = "lib.rs"
matches = true
case_insensitive = true
literal_separator = true
backslash_escape = true
"#;

        let mut tests = GlobTests::new();
        tests.load_slice("test", data.as_bytes()).unwrap();

        let t0 = &tests.tests[0];
        assert_eq!("test", t0.group());
        assert_eq!("foo", t0.name());
        assert_eq!("test/foo", t0.full_name());
        assert_eq!(&["*.rs"], t0.globs());
        assert_eq!(true, t0.is_match());
        assert_eq!(&[0], t0.which_matches());
    }

    #[test]
    fn load2() {
        let data = r#"
[[tests]]
name = "foo"
glob = ["*.rs", "*.toml"]
input = "lib.rs"
matches = [0, 2, 5]
case_insensitive = true
literal_separator = true
backslash_escape = true
"#;

        let mut tests = GlobTests::new();
        tests.load_slice("test", data.as_bytes()).unwrap();

        let t0 = &tests.tests[0];
        assert_eq!(&["*.rs", "*.toml"], t0.globs());
        assert_eq!(true, t0.is_match());
        assert_eq!(&[0, 2, 5], t0.which_matches());
    }
}

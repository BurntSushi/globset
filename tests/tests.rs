extern crate glob_test;
extern crate globset;

use std::error::Error;

use globset::{Glob, GlobBuilder, GlobSetBuilder};

use glob_test::bstr::{BString, ByteSlice};
use glob_test::{CompiledGlob, GlobTest, GlobTests, TestResult, TestRunner};

#[test]
fn matcher() -> Result<(), Box<dyn Error>> {
    TestRunner::new()?
        .test_iter(all_tests()?.iter(), compile)
        .assert_verbose();
    Ok(())
}

// This runs all single-glob tests on a GlobSet, and skips anything else,
// since they are handled by `matcher`.
#[test]
fn matcher_set() -> Result<(), Box<dyn Error>> {
    TestRunner::new()?
        .test_iter(all_tests()?.iter(), |test, globs| {
            if globs.len() > 1 {
                Ok(CompiledGlob::skip())
            } else {
                compile_many(test, globs)
            }
        })
        .assert_verbose();
    Ok(())
}

fn all_tests() -> Result<GlobTests, Box<dyn Error>> {
    let mut tests = GlobTests::new();
    macro_rules! load {
        ($name:expr) => {{
            const DATA: &[u8] =
                include_bytes!(concat!("data/", $name, ".toml"));
            tests.load_slice($name, DATA)?;
        }};
    }

    load!("literal");
    load!("star");
    load!("recursive");
    load!("range");
    load!("unicode");
    load!("escape");
    load!("case");
    load!("alt");
    load!("error");
    if cfg!(windows) {
        load!("windows");
    } else if cfg!(unix) {
        load!("unix");
    }

    load!("literal-set");

    Ok(tests)
}

fn compile(
    test: &GlobTest,
    globs: &[BString],
) -> Result<CompiledGlob, Box<dyn Error>> {
    if globs.len() > 1 {
        compile_many(test, globs)
    } else {
        compile_one(test, &globs[0])
    }
}

fn compile_many(
    test: &GlobTest,
    globs: &[BString],
) -> Result<CompiledGlob, Box<dyn Error>> {
    let mut setb = GlobSetBuilder::new();
    for pattern in globs {
        let glob = build_glob(test, pattern)?;
        setb.add(glob);
    }
    setb.build().map_err(From::from).map(|set| {
        CompiledGlob::compiled(move |test| -> TestResult {
            let input = match test.input().to_str() {
                Err(_) => return TestResult::skip(),
                Ok(input) => input,
            };
            TestResult::which(set.matches(input))
        })
    })
}

fn compile_one(
    test: &GlobTest,
    glob: &[u8],
) -> Result<CompiledGlob, Box<dyn Error>> {
    build_glob(test, glob).map(|g| {
        let matcher = g.compile_matcher();
        CompiledGlob::compiled(move |test| -> TestResult {
            let input = match test.input().to_str() {
                Err(_) => return TestResult::skip(),
                Ok(input) => input,
            };
            if matcher.is_match(input) {
                TestResult::matched()
            } else {
                TestResult::no_match()
            }
        })
    })
}

fn build_glob(test: &GlobTest, glob: &[u8]) -> Result<Glob, Box<dyn Error>> {
    GlobBuilder::new(glob.to_str()?)
        .case_insensitive(test.case_insensitive())
        .literal_separator(test.literal_separator())
        .backslash_escape(test.backslash_escape())
        .build()
        .map_err(From::from)
}

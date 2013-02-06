{- Author: TODO: WRITE YOUR NAME HERE
 - Email:  TODO: WRITE YOUR EMAIL HERE
 -
 - CS / CE 4337 Spring 2013 Sections 001, 002
 -
 - Assignment 1:   Introduction to Functional Programming with Haskell
 - Assigned:       Tuesday, 5 Feb 2013
 - Due:            Tuesday, 12 Feb 2013, 23:59 (11:59pm)
 - Estimated SLOC: 31
 -
 -
 - **DON'T FORGET TO FILL IN YOUR NAME AND EMAIL ABOVE!**
 -
 -
 - **WARNING!**
 -
 - Before submission, make sure to search the file for "TODO" to make sure you
 - have completed all tasks!
 -
 - **end WARNING!!!**
 -
 -
 - For each function, you **MUST** also supply the type signature!
 -
 -
 -
 - Tips
 - ====
 -
 -  *  The type system really is your friend --- it will catch errors at
 -     compile time that most other compilers defer to runtime. When you get
 -     such a type error, consider what type you are expecting the affected
 -     expressions to have.  You can require that the expression be of a
 -     particular type by wrapping the expression in parentheses and attaching
 -     `:: TypeName` to it. For example:
 -
 -        (x :: Integer)
 -     
 -     The type checker will have to ensure whether the `x` above is of type
 -     `Integer` when compiling your code. If it is not, you should get some
 -     diagnostics of what type it has inferred for `x`, which may help you
 -     track down your problem.
 -
 -  *  Similarly, if you receive type errors of the form 
 -   
 -         Couldn't match expected type 'SomeType'
 -                     with actual type 'SomeOtherType -> SomeType'
 -
 -     then you probably forgot to include some parameter to a function call.
 -     Remember: Haskell functions are Curried, so you are not *required* to
 -     supply all parameters when invoking a function (unlike C, for example).
 -
 -
 - Running your code
 - =================
 -
 - Download the Haskell Platform from http://www.haskell.org/platform/ .
 -  *  NOTE: For those using Linux, you should install Haskell via your
 -     package manager. You will also need to install the HUnit package, and
 -     you may also wish to install Haddock. This will make uninstallation
 -     easier (but why would you want to do that?).
 -
 - To receive credit, your code should load in the GHCi interactive
 - interpreter without error. That is, running the following command (note:
 - the '%' is the prompt character; you shouldn't type it):
 - 
 -     % ghci intro.hs
 -
 - should produce the output
 -
 -     GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
 -     Loading package ghc-prim ... linking ... done.
 -     Loading package integer-gmp ... linking ... done.
 -     Loading package base ... linking ... done.
 -     [1 of 1] Compiling Main             ( intro.hs, interpreted )
 -     Ok, modules loaded: Main.
 -     *Main> 
 - 
 - The last line may also read `Prelude Main>`.
 -
 - You can then execute the functions written in the file directly:
 -
 -     *Main> collatzList 5
 -
 - You can run individual test suites via the `runTestTT` function with the
 - name of the test suite (here, they are named `fn_tests`, where `fn` is the
 - name of the function being tested:
 -
 -     *Main> runTestTT collatzList_tests
 -
 - You can also run all of the test suites via `main`
 -
 -     *Main> main
 -
 - None of the test cases should fail --- the final output of running each
 - test suite should contain "Failures: 0".
 -  *  Your final output should contain "Errors: 0" as well, especially for
 -     test cases that are not expected to throw an error (via the `error`
 -     function).
 -
 -  *  If an error is reported for a case using `assertError`, then PLEASE let
 -     me know about it.
 -
 -
 - Finally, you can also compile your program into an executable:
 -
 -     % ghc intro.hs
 -
 - This will produce an executable named `intro`; running this executable will
 - run the test cases.
 -}



-- Scaffolding
-- ============================================================================

-- These imports are for the HUnit extension to check that an exception was
-- thrown
import Control.Exception
import Control.Monad
import Control.DeepSeq

-- HUnit import
import Test.HUnit



-- Tells Haskell that two exceptions are equal if their strings are equal.
instance Eq ErrorCall where
    x == y = (show x) == (show y)


-- | A HUnit assertion to test that a given call throws an exception.
--
--   Taken from 
--   http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks/6147930#6147930
--   and 
--   http://stackoverflow.com/questions/13350164/how-do-i-test-for-an-error-in-haskell
--
--   NOTE: This may not work if the exception is not thrown directly from the
--   test case expression. For example, if you are testing that a function `f`
--   throws an exception `E`, and `E` is actually thrown by `g` (a function
--   called by `f`), the exception may not be caught, causing a failing test.
--
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex) 


-- | An HUnit assertion to test that a given call throws an error.
--   
--   Based on `assertException`. However, this only handles calls to `error`,
--   not exceptions in general. The actual error message is discarded; the
--   only thing tested is that an error was thrown, not whether a particular
--   error *message* was thrown. 
--
--   It appears to work better than `assertException` above, in that if the
--   error is thrown by a function called by the function under testing `f`,
--   the thrown error will still be detected and the test will pass. This is
--   done via the `force` function from the `Control.DeepSeq` module.
--
assertError :: ( NFData a, Show a ) => String -> a -> Assertion
assertError name f = 
    handle (\ (ErrorCall _) -> return () ) $ do
        res <- evaluate action
        assertFailure $ unlines [ name
                                , "expected: error message"
                                , " but got: " ++ show res
                                ]
    where 
        action  =  force f




-- Examples
-- ============================================================================

{- | Squares its integer argument
 -}
sq :: Integer -> Integer
sq x = 
    x * x

-- Example test cases
-- ----------------------------------------------------------------------------

sq_test0 = 
    TestCase ( assertEqual  -- The call's result must equal the expected value
                "sq 0"      -- Name of the test; used when printing results
                0           -- The expected value
                ( sq 0 )    -- The code that should eval to the expected value
             )

sq_test1 =
    TestCase ( assertEqual
                "sq 1"
                2
                ( sq 1 )
             )


-- A list of tests; `runTestTT sq_tests` would run all of these.
sq_tests =
    TestList 
        [ TestLabel "sq_test0" sq_test0
        , TestLabel "sq_test1" sq_test1
        ]


{- | Squares its two `Integer` arguments and adds them together.
 -}
sq_two_add :: Integer -> Integer -> Integer
sq_two_add x y =
    a + b
    where
        a = x * x
        b = y * y


{- | Returns `True` if `x` is a member of the list `l` and `False` otherwise.
 -}
-- This example uses the guard notation for choices. You could also write this
-- using pattern matching, as done for `sum_list` below.
member :: Integer -> [Integer] -> Bool
member x l  
    |  l == []          =  False
    |  ( head l ) == x  =  True
    |  otherwise        =  member x (tail l)


{- | Takes a list of `Integer`s and returns their sum.
 -}
-- This example uses pattern matching to split up the list into its head and
-- tail --- i.e., matching `l` with `( x : xs )` binds `x` to the head and `xs`
-- to the tail.
sumList :: [Integer] -> Integer
sumList l =
    case l of
        []      ->  0
        x : xs  ->  x + (sumList xs)


{- | The usual factorial calculation.
 -
 -   NOTE: This code is taken from _Learn You a Haskell for Great Good!_,
 -   which does **NOT** ensure that the argument is not a negative integer.
 -   Could you extend this to handle this case?
 -}
-- This example uses pattern matching on the arguments and shows that multiple
-- equations can be defined for a single function. The equations are checked
-- top-to-bottom.
--
factorial :: Integer -> Integer
factorial 0  =  1
factorial n  =  n * factorial ( n - 1 )



{- | The usual Fibonacci calculation.
 -}
-- Observe the use of the infix `( || )` operator for disjunction.
fibonacci :: Integer -> Integer
fibonacci n 
    | n < 0             =  error "Illegal negative number input to fibonacci"
    | n == 0 || n == 1  =  n
    | n > 1             =  ( fibonacci ( n - 1 ) ) + ( fibonacci ( n - 2 ) )


fibonacci_testNeg1 =
    TestCase ( assertError
                    "fibonacci ( -1 )"
                    ( fibonacci ( -1 ) )
             )

{- | Computes the list of Fibonacci numbers starting at the `n`th and
 -   proceeding down to the 0th.
 -
 -   NOTE: If `n` < 0, an error is thrown.
 -
 -   NOTE: This is obviously horribly inefficient, so don't throw huge numbers
 -   at it. We will see a MUCH more efficient version of this function later
 -   in the course.
 -}
fibList :: Integer -> [Integer]
fibList n
    | n < 0      =  error "Illegal negative number input to fibList"
    | n == 0     =  [ fibonacci n ]
    | otherwise  =  ( fibonacci n ) : ( fibList ( n - 1 ) )


-- Test Cases
-- ----------------------------------------------------------------------------
-- 

fibList_test0 = 
    TestCase ( assertEqual
                    "fibList 0"
                    [ 0 ]
                    ( fibList 0 )
             )

fibList_test1 =
    TestCase ( assertEqual
                    "fibList 1"
                    [ 1, 0 ]
                    ( fibList 1 )
             )


fibList_testNeg1 =
    TestCase ( assertError 
                    "fibList ( -1 )"
                    ( fibList ( -1 ) ) 
             )


fibList_test8 =
    TestCase ( assertEqual
                    "fibList 8"
                    [ 21, 13, 8, 5, 3, 2, 1, 1, 0 ]
                    ( fibList 8 )
             )


fibList_tests =
    TestList [ TestLabel "fibList_test0"    fibList_test0
             , TestLabel "fibList_test1"    fibList_test1
             , TestLabel "fibList_testNeg1" fibList_testNeg1
             , TestLabel "fibList_test8"    fibList_test8
             ]



-- Homework problems
-- ============================================================================


{- Problem 0: `sumRange`
 - ============================================================================
 -
 - Write a function `sumRange` that takes two `Integer` arguments `x` and `y`
 - representing a range and sums the `Integer`s that lie in that range,
 - inclusive. 
 -
 - Note that the ordering of the arguments is undefined, so your code should
 - not only handle the case where `x` <= `y`, but the case where `x > y` as
 - well. We can thus represent this using Dijkstra's quantifier notation as
 -
 -     << Sum i  :  (min x y) <= i <= (max x y)  :  i >>
 -
 - 
 - Notes
 - -----
 -
 -  *  There are a few ways that you can choose to handle the "inverse"
 -     ordering of `x` > `y`. I would suggest one of the following:
 -      *  Add the appropriate checks to your function, recursively calling
 -         the function with the arguments reversed if appropriate. Recall
 -         that in `case` statements and guards, the expressions are evaluated
 -         top-to-bottom, so take this into account when ordering your cases.
 -
 -      *  Have the "top-level" `sumRange` call a helper function (e.g.,
 -         `sumRange'`) with the appropriate values and push the actual
 -         recursive solution to this helper function.
 -
 -     NOTE: You should be able to write both of these and demonstrate that
 -     they work!
 -
 -  *  The arguments may be negative, so your function should support these
 -     values. This shouldn't be a problem if you just focus on writing the
 -     solution to the problem, as we discussed in class --- i.e., you don't
 -     need to add special code to handle this case, just make sure not to add
 -     code forbidding it.
 -
 -  *  As noted by the evening class, the original form of the problem
 -     statement, namely
 -
 -         << Sum i  :  x <= i <= y  :  i >>
 -     
 -     excludes the case where `x` > `y`. For completeness, observe that the
 -     range in this case would be empty --- what value should be returned?
 -}

-- TODO: WRITE YOUR CODE HERE


-- Test Cases
-- ---------------------------------------------------------------------------- 

sumRange_test0 =
    TestCase ( assertEqual
                    "sumRange 0 0"
                    0
                    ( sumRange 0 0 )
             )

sumRange_testEq =
    TestCase ( assertEqual
                    "sumRange 5 5"
                    5
                    ( sumRange 5 5 )
             )

sumRange_testNegNeg =
    TestCase ( assertEqual
                    "sumRange ( -10 ) ( -5 )"
                    ( -45 )
                    ( sumRange ( -10 ) ( -5 ) )
             )

sumRange_testPosPos =
    TestCase ( assertEqual
                    "sumRange 10 15"
                    75
                    ( sumRange 10 15 )
             )

sumRange_testNegPos =
    TestCase ( assertEqual
                    "sumRange ( -10 ) 10"
                    0
                    ( sumRange ( -10 ) 10 )
             )

sumRange_testInvPos =
    TestCase ( assertEqual
                    "sumRange 10 8"
                    27
                    ( sumRange 10 8 )
             )

sumRange_testInvNeg =
    TestCase ( assertEqual
                    "sumRange ( -7 ) ( -12 )"
                    ( -57 )
                    ( sumRange ( -7 ) ( -12 ) )
             )

sumRange_testInvPosNeg =
    TestCase ( assertEqual
                    "sumRange 10 ( -12 )"
                    ( -23 )
                    ( sumRange 10 ( -12 ) )
             )


sumRange_tests =
    TestList [ TestLabel "sumRange_test0"         sumRange_test0
             , TestLabel "sumRange_testEq"        sumRange_testEq
             , TestLabel "sumRange_testNegNeg"    sumRange_testNegNeg
             , TestLabel "sumRange_testPosPos"    sumRange_testPosPos
             , TestLabel "sumRange_testNegPos"    sumRange_testNegPos
             , TestLabel "sumRange_testInvPos"    sumRange_testInvPos
             , TestLabel "sumRange_testInvNeg"    sumRange_testInvNeg
             , TestLabel "sumRange_testInvPosNeg" sumRange_testInvPosNeg
             ]



{- Problem 1: Euclid's Greatest Common Divisor Algorithm
 - ============================================================================
 -
 - Write a function `euclidGCD` that takes two `Integer` arguments `x` and `y`
 - and computes their greatest common divisor using Euclid's algorithm.
 -
 - Euclid's algorithm can be defined as follows:
 -  *  If `x` == 0, then y
 -  *  If `y` == 0, then x
 -  *  If `x` == `y`, then `x` (or `y`, since they're equal)
 -  *  If `x` > `y`, then the GCD of `x - y` and `y`
 -  *  If `x` < `y`, then the GCD of `x` and `y - x`.
 -
 - **WARNING**
 -
 -     The above definition differs from the version discussed in class.
 -     Read the above description carefully before beginning! (The differences
 -     are explained in the "Notes" below)
 -
 - end **WARNING**
 - 
 -
 - Notes
 - -----
 -
 -  *  After consideration (still ongoing), it is worth noting that the GCD of
 -     `x` and `0` is `x` --- `x` evenly divides both `x` and `0` with no
 -     remainder.
 -
 -  *  Per a note in the Wikipedia article on GCD, we will take 
 -     ( gcd 0 0 ) = 0 for simplicity, though it does bug me a bit. I'm still
 -     looking into this, but it is worth noting that the Haskell-provided
 -     `gcd` function does behave this way.
 -      *  See the note at
 -         http://en.wikipedia.org/wiki/Greatest_common_divisor#Properties
 - 
 -  *  It is worth noting that the change to the answers for 0 make one of the
 -     above rules unnecessary. You are encouraged to change your code
 -     accordingly, but make sure you can show that your change is correct!
 -
 -  *  Recall that you should also be able to handle negative numbers. You
 -     will have to adjust the above outline to support them. Consider the
 -     suggestions for handling the "inverse" ordering of `sumRange` above.
 -}

-- TODO: WRITE YOUR CODE HERE



-- Test Cases
-- ----------------------------------------------------------------------------

euclidGCD_test00 =
    TestCase ( assertEqual
                    "euclidGCD 0 0"
                    0
                    ( euclidGCD 0 0 )
             )

euclidGCD_test01 =
    TestCase ( assertEqual
                    "euclidGCD 0 1"
                    1
                    ( euclidGCD 0 1 )

             )

euclidGCD_test50 =
    TestCase ( assertEqual
                    "euclidGCD 5 0"
                    5
                    ( euclidGCD 5 0 )
             )

euclidGCD_testPosEq =
    TestCase ( assertEqual
                    "euclidGCD 10 10"
                    10
                    ( euclidGCD 10 10 )
             )

euclidGCD_testNegEq =
    TestCase ( assertEqual
                    "euclidGCD ( -5 ) ( -5 )"
                    5
                    ( euclidGCD ( -5 ) ( -5 ) )
             )

euclidGCD_testNegPos =
    TestCase ( assertEqual
                    "euclidGCD 15 ( -12 )"
                    3
                    ( euclidGCD 15 ( -12 ) )
             )

euclidGCD_testPrime =
    TestCase ( assertEqual
                    "euclidGCD 7 19"
                    1
                    ( euclidGCD 7 19 )
             )

euclidGCD_testCoPrime =
    TestCase ( assertEqual
                    "euclidGCD 16 25"
                    1
                    ( euclidGCD 16 25 )
             )


euclidGCD_tests =
    TestList [ TestLabel "euclidGCD_test00"      euclidGCD_test00
             , TestLabel "euclidGCD_test01"      euclidGCD_test01
             , TestLabel "euclidGCD_test50"      euclidGCD_test50
             , TestLabel "euclidGCD_testPosEq"   euclidGCD_testPosEq
             , TestLabel "euclidGCD_testNegEq"   euclidGCD_testNegEq
             , TestLabel "euclidGCD_testNegPos"  euclidGCD_testNegPos
             , TestLabel "euclidGCD_testPrime"   euclidGCD_testPrime
             , TestLabel "euclidGCD_testCoPrime" euclidGCD_testCoPrime
             ]


{- Problem 2: `collatz`
 - ============================================================================
 - 
 - Write a function `collatz` that takes an `Integer` argument `x` and returns
 - the value in the Collatz sequence that follows `x`. Recall that the Collatz
 - function is defined as follows:
 -  *  If `x` is even, then x / 2
 -  *  If `x` is odd, then 3 * x + 1
 -
 - Notes
 - -----
 -
 -  *  Recall that the Collatz function is only defined on positive integers.
 -     Therefore, if `x` < 1, you should use the `error` function to indicate
 -     that the caller supplied an invalid value. 
 -
 -  *  Recall that you can use the `mod` function to perform the modulus
 -     operation. You can represent such functions as infix operators by
 -     surrounding them with backticks (NOT APOSTROPHES!), as follows:
 -
 -         5 `mod` 2    -- computes 5 modulo 2 = 1
 -
 -  *  Recall that the `( / )` operator is floating-point division, which will
 -     give a type error when used with `Integer` arguments. Instead, you will
 -     want to use the integer division function `div`, as follows:
 -
 -         7 `div` 2    -- computes 7 divided by 2, truncating the remainder,
 -                      -- so the result is 3
 -
 -  *  There is no recursion going on here --- that's the next function. Just
 -     define `collatz` according to this spec.
 -
 -  *  You should not attempt to handle 1 specially; `collatz 1` should apply
 -     the case where `x` is odd.
 -}

-- TODO: WRITE YOUR CODE HERE


-- Test Cases
-- ----------------------------------------------------------------------------

collatz_test1 =
    TestCase ( assertEqual
                    "collatz 1"
                    4
                    ( collatz 1 )
             )


-- NOTE: When this test case is run, it may display that an error or failure
-- occurred. If it does, please let me know.
collatz_test0 =
    TestCase ( assertError
                    "collatz 0"
                    ( collatz 0 )
             )

-- NOTE: When this test case is run, it may display that an error or failure
-- occurred. If it does, please let me know.
collatz_testNeg5 =
    TestCase ( assertError
                    "collatz ( -5 )"
                    ( collatz ( -5 ) )
             )


collatz_test7 =
    TestCase ( assertEqual
                    "collatz 7"
                    22
                    ( collatz 7 )
             )

collatz_test8 =
    TestCase ( assertEqual
                    "collatz 8"
                    4
                    ( collatz 8 )
             )

collatz_test12 =
    TestCase ( assertEqual
                    "collatz 12"
                    6
                    ( collatz 12 )
             )

collatz_test51 =
    TestCase ( assertEqual
                    "collatz 51"
                    154
                    ( collatz 51 )
             )



collatz_tests =
    TestList [ TestLabel "collatz_test1"    collatz_test1
             , TestLabel "collatz_test0"    collatz_test0
             , TestLabel "collatz_testNeg5" collatz_testNeg5
             , TestLabel "collatz_test7"    collatz_test7
             , TestLabel "collatz_test8"    collatz_test8
             , TestLabel "collatz_test12"   collatz_test12
             , TestLabel "collatz_test51"   collatz_test51
             ]




{- Problem 3: `collatzList`
 - ============================================================================
 -
 - Write a function `collatzList` that takes an `Integer` argument `x` and
 - returns a list of `Integer`s corresponding to the Collatz sequence
 - beginning at `x` and terminating at `1`.
 -
 - Notes
 - -----
 -
 -  *  You should use `collatz` above to obtain the next value in the Collatz
 -     sequence, so as not to duplicate work.
 -
 -  *  You can choose whether to check that `x` >= 1 here in addition to
 -     `collatz` (preferable), or just rely on `collatz` to throw the error if
 -     an invalid value is entered.
 -      *  The unit test should catch the exception in both cases; if it does
 -         not, please let me know.
 -
 -  *  Recall that we can use the `( : )` operator to build lists; for example, 
 -     `5 : [ 1, 2, 3, 4 ]` will result in the list `[ 5, 1, 2, 3, 4 ]`.
 -     Likewise, an expression that evaluates to a list can be placed on the
 -     right-hand side of the `( : )`; in such cases, the result of this
 -     expression will form the tail of the list.
 -}

-- TODO: WRITE YOUR CODE HERE


-- Test Cases
-- ---------------------------------------------------------------------------

collatzList_test1 = 
    TestCase ( assertEqual 
                    "collatzList 1"
                    [1]
                    ( collatzList 1 )
             )

-- NOTE: When this test case is run, it may display that an error or failure
-- occurred. If it does, please let me know.
collatzList_test0 = 
    TestCase ( assertError
                    "collatzList 0" 
                    ( collatzList 0 )
             )

-- NOTE: When this test case is run, it may display that an error or failure
-- occurred. If it does, please let me know.
collatzList_testNeg8 = 
    TestCase ( assertError
                    "collatzList ( -8)" 
                    ( collatzList ( -8 ) )
             )

collatzList_test7 = 
    TestCase ( assertEqual
                    "collatzList 7" 
                    [ 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4,
                      2, 1 ] 
                    ( collatzList 7 )
             )

collatzList_test8 =
    TestCase ( assertEqual
                    "collatzList 8"
                    [ 8, 4, 2, 1 ]
                    ( collatzList 8 )
             )

collatzList_test12 =
    TestCase ( assertEqual
                    "collatzList 12"
                    [ 12, 6, 3, 10, 5, 16, 8, 4, 2, 1 ]
                    ( collatzList 12 )
             )

collatzList_test51 = 
    TestCase ( assertEqual
                    "collatzList 51"
                    [ 51, 154, 77, 232, 116, 58, 29, 88, 44, 22, 11, 34, 17,
                      52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1 ]
                    ( collatzList 51 )
             )



collatzList_tests = 
    TestList [ TestLabel "collatzList_test1"    collatzList_test1
             , TestLabel "collatzList_test0"    collatzList_test0
             , TestLabel "collatzList_testNeg8" collatzList_testNeg8
             , TestLabel "collatzList_test7"    collatzList_test7
             , TestLabel "collatzList_test8"    collatzList_test8
             , TestLabel "collatzList_test12"   collatzList_test12
             , TestLabel "collatzList_test51"   collatzList_test51
             ]



-- Main (just runs the unit tests
-- ==============================================================================

main = do
    putStrLn "Running 'sumRange' tests"
    runTestTT sumRange_tests
    putStrLn ""

    putStrLn "Running 'euclidGCD' tests"
    runTestTT euclidGCD_tests
    putStrLn ""

    putStrLn "Running 'collatz' tests"
    runTestTT collatz_tests
    putStrLn ""

    putStrLn "Running 'collatzList' tests"
    runTestTT collatzList_tests
    putStrLn ""

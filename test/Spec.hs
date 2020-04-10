import Test.HUnit

main :: IO Counts
main = runTestTT tests

testAddition = TestCase $ assertEqual "4+5=9" (4+5) 9

tests = TestList [TestLabel "testAddition" testAddition]

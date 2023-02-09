// PROGRAM CODE 42
import "tlang/std.tl"
main :: () Int {
    assert #file.ends_with("location.tl")
    assert #line == 5
    assert #location.ends_with("location.tl:6:12")
    assert #function == "() Int"
    assert #container == "main"
    return 42
}
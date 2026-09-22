module Tests

open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
let ``My theory`` (n: int) = Assert.True(n > 0)

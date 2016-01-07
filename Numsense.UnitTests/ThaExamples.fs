module Ploeh.Numsense.ThaiExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("ศูนย์", 0)>]
[<InlineData("หนึ่ง", 1)>]
[<InlineData("สอง", 2)>]
[<InlineData("สาม", 3)>]
[<InlineData("สี่", 4)>]
[<InlineData("ห้า", 5)>]
[<InlineData("หก", 6)>]
[<InlineData("เจ็ด", 7)>]
[<InlineData("แปด", 8)>]
[<InlineData("เก้า", 9)>]
[<InlineData("สิบ", 10)>]
[<InlineData("สิบเอ็ด", 11)>]
[<InlineData("สิบสอง", 12)>]
[<InlineData("สิบสาม", 13)>]
[<InlineData("สิบสี่", 14)>]
[<InlineData("สิบห้า", 15)>]
[<InlineData("สิบหก", 16)>]

[<InlineData("หนึ่งร้อย", 100)>]
[<InlineData("หนึ่งร้อยเอ็ด", 101)>]
[<InlineData("หนึ่งร้อยสิบสอง", 112)>]

let `` tryOfThai `` (thai, expected) =
    let actual = Numeral.tryParseThai thai
    Some expected =! actual
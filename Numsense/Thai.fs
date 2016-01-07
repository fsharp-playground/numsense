module internal Ploeh.Numsense.Thai

open Ploeh.Numsense.InternalDsl

let rec internal toThaiImp x =
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0 then prefix
        else sprintf "%s-%s" prefix (toThaiImp (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toThaiImp (x / factor)) suffix
        simplify prefix factor x

    match x with
    | x when x < 0 -> sprintf "ลบ %s" (toThaiImp -x)
    | 0 -> "ศูนย์"
    | 1 -> "หนึ่ง"
    | 2 -> "สอง"
    | 3 -> "สาม"
    | 4 -> "สี่"
    | 5 -> "ห้า"
    | 6 -> "หก"
    | 7 -> "เจ็ด"
    | 8 -> "แปด"
    | 9 -> "เก้า"
    | 10 -> "สิบ"
    | 11 -> "สิบเอ็ด"
    | 12 -> "สิบสอง"
    | 13 -> "สิบสาม"
    | 14 -> "สิบสี่"
    | 15 -> "สิบห้า"
    | 16 -> "สิบหก"
    | 17 -> "สิบเจ็ด"
    | 18 -> "สิบแปด"
    | 19 -> "สิบเก้า"
    | Between 20 30 x -> simplify "ยี่สิบ" 10 x
    | Between 30 40 x -> simplify "ยี่สิบ" 10 x
    | Between 40 50 x -> simplify "ยี่สิบ" 10 x
    | Between 60 70 x -> simplify "ยี่สิบ" 10 x
    | Between 80 30 x -> simplify "ยี่สิบ" 10 x
    | Between 90 100 x -> simplify "ยี่สิบ" 10 x
    | Between 100 1000 x -> format "" 100 x
    | _ -> ""

let internal tryParseThaiImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | "" -> Some acc
        | StartsWith "-" t 
        | StartsWith "ศูนย์" t -> conv (0 + acc) t
        | StartsWith "หนึ่ง" t -> conv (1 + acc) t
        | StartsWith "สอง" t -> conv (2 + acc) t
        | StartsWith "สาม" t -> conv (3 + acc) t
        | StartsWith "สี่" t -> conv (4 + acc) t
        | StartsWith "ห้า" t -> conv (5 + acc) t
        | StartsWith "หก" t -> conv (6 + acc) t
        | StartsWith "เจ็ด" t -> conv (7 + acc) t
        | StartsWith "แปด" t -> conv (8 + acc) t
        | StartsWith "เก้า" t -> conv (9 + acc) t
        | StartsWith "สิบ" t -> conv (10 + acc) t
        | StartsWith "ร้อย" t -> conv (if acc = 0 then 100 else 100 %* acc) t
        | _ -> None

    //let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "th")
    match x with
    | StartsWith "ลบ" t -> conv 0 (t.Trim()) |> Option.map ((+) -1)
    | _ -> conv 0 x
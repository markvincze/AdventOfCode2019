let password number =
    let rec password number previousDigit hadDouble =
        if number = 0
        then hadDouble
        else let digit = number % 10
             if digit > previousDigit
             then false
             else if digit = previousDigit
                  then password (number / 10) digit true
                  else password (number / 10) digit hadDouble
    password (number / 10) (number % 10) false

let result1 = [152085..670283]
              |> Seq.filter password
              |> Seq.length

let password2 number =
    let rec password2 number d1 d2 d3 hadDouble =
        if number = 0
        then hadDouble || (Option.get d1) = (Option.get d2) && (Option.get d2) <> (Option.get d3)
        else let newDigit = number % 10
             match newDigit, d1, d2, d3 with
             | newDigit, None, None, None -> password2 (number / 10) (Some newDigit) None None false
             | newDigit, Some d1, _, _ when newDigit > d1 -> false
             | newDigit, Some d1, None, None -> password2 (number / 10) (Some newDigit) (Some d1) None false
             | newDigit, Some d1, Some d2, None when newDigit <> d1 && d1 = d2
                 -> password2 (number / 10) (Some newDigit) (Some d1) (Some d2) true
             | newDigit, Some d1, Some d2, None -> password2 (number / 10) (Some newDigit) (Some d1) (Some d2) false
             | newDigit, Some d1, Some d2, Some d3 when newDigit <> d1 && d1 = d2 && d2 <> d3
                 -> password2 (number / 10) (Some newDigit) (Some d1) (Some d2) true
             | newDigit, Some d1, Some d2, Some _ -> password2 (number / 10) (Some newDigit) (Some d1) (Some d2) hadDouble
             | _ -> failwith "Invalid state"
    password2 number None None None false

let result2 = [152085..670283]
              |> Seq.filter password2
              |> Seq.length

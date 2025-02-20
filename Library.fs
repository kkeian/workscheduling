namespace workscheduling

module Scheduling =
    type Timespan =
        {
            Start : System.TimeOnly
            End : System.TimeOnly
        }

    let parseToTimespan (start : string) (endhr : string) =
        {
            Start = System.TimeOnly.Parse start
            End = System.TimeOnly.Parse endhr
        }

    type DayOfWeek =
        | Monday = 1
        | Tuesday = 2
        | Wednesday = 3
        | Thursday = 4
        | Friday = 5
        | Saturday = 6
        | Sunday = 7

    type OpenHours =
        {
            Day : DayOfWeek array
            Times : Timespan array
        }

    let makeOpenHours (wkdaySpan : Timespan) (wkendSpan : Timespan) =
        {
            Day = [|
                for day in 1..7 do enum<DayOfWeek> day
            |]
            Times = Array.append
                [| for i in 1..5 do wkdaySpan |]
                [| for i in 1..2 do wkendSpan |]
        }

    type Address =
        {
            Number : uint
            StreetName : string
            SuiteNumber : uint
            Floor : int
            City : string
            State : string
            ZipCode : uint
        }

    type Location =
        {
            Name : string
            Address : Address
            OpenHours : OpenHours
        }

    // generator function that returns next valid Id
    type Id = Id of uint32
    // closure to make sure each id is unique
    let idGenerator () =
        let mutable id = Id 0u
        let unwrapId (Id i) = i
        fun () ->
            id <- Id ((id |> unwrapId) + 1u)
            id

    let nextPosNum = idGenerator ()
    type Position =
        {
            Name : string
            Id : Id // should be unique
        }

    module EmployeeTypes =
        let AD = { Name = "Assistant Director"; Id = nextPosNum () }
        let Sensei = { Name = "Sensei"; Id = nextPosNum () }

    let nextEmployeeNum = idGenerator ()
    type Employee =
        { 
            FirstName : string
            LastName : string
            //Availability : Schedule
            //Schedule : Schedule
            EmployeeNumber : Id
            JobTitle : Position
            Priority : uint32
            DesiredWeeklyHours : uint8
        }

    let makeEmployee first last pos pri desiredhrs =
        { 
            FirstName = first
            LastName = last
            EmployeeNumber = nextEmployeeNum ()
            JobTitle = pos
            Priority = pri
            DesiredWeeklyHours = desiredhrs
        }

    type TimeSlot =
        {
            TimeBlock : Timespan
            Location : Location
            EmployeesOnShift : Employee list
            KidsScheduled : uint8
        }

    let chooseEmployee e1 e2  =
        if e1.Priority < e2.Priority then e1
        else e2

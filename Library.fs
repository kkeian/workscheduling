namespace workscheduling

module Scheduling =
    type TimeSpan =
        {
            Start : System.TimeOnly
            End : System.TimeOnly
        }

    type Day =
        {
            Date : System.DateOnly
            TimeBlock : TimeSpan
        }

    // use to instantiate Schedule Week
    let makeWeekDays (startDate: System.DateOnly ) =
        [| for i in 0 .. 6 -> startDate.AddDays i |]

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
            Addr : Address
            OpenHours : Day array
        }

    type Schedule =
        {
            Week : Day array
            Site : Location
        }
    
    type Position =
        {
            Name : string
            Id : uint32 seq // should be unique
        }

    // generator function that returns next valid Id
    let validIds = seq { for i in 1u .. System.UInt32.MaxValue -> i }
    let makePosition name =
        {
            Name = name
            Id = validIds
        }

    type Employee =
        { 
            FirstName : string
            LastName : string
            Availability : Schedule
            WeekSchedule : Schedule
            EmployeeNumber : uint64
            JobTitle : Position
        }

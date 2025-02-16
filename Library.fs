namespace workscheduling

module Scheduling =
    type Day =
        {
            Date : System.DateOnly
            TimeBlock : System.TimeOnly
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

    type Employee =
        { 
            FirstName : string
            LastName : string
            Availability : Schedule
            WeekSchedule : Schedule
            EmployeeNumber : uint64
        }

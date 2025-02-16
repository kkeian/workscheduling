namespace workscheduling

module Scheduling =
    type TimeSpan =
        {
            Start : System.TimeOnly
            End : System.TimeOnly
        }

    let getHours (t: TimeSpan) =
        let timeBlock = (t.End - t.Start)
        float timeBlock.Hours + (float timeBlock.Minutes / 60.0)

    type Day =
        {
            Date : System.DateOnly
            Hours : TimeSpan
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

    // DU to enforce type. to use Days inside Week
    //  unwrap week
    type Week = Week of Day array
    let unwrapWeek (Week w) = w

    type Schedule =
        {
            Week : Week
            Location : Location
        }
    
    // generator function that returns next valid Id
    type Id = Id of uint32
    // closure to make sure each id is unique
    let idGenerator =
        let mutable id = Id 0u
        let incr =
            let unwrapId (Id i) = i
            id <- Id ((id |> unwrapId) + 1u)
            id
        incr

    let nextId = idGenerator

    type Position =
        {
            Name : string
            Id : Id // should be unique
        }

    let makePosition name =
        {
            Name = name
            Id = nextId
        }

    type Employee =
        { 
            FirstName : string
            LastName : string
            Availability : Schedule
            Schedule : Schedule
            EmployeeNumber : uint64
            JobTitle : Position
            Priority : uint32
            DesiredWeeklyHours : uint8
        }

    let totalHoursScheduled (e: Employee) =
        let mutable totalHrs = 0.0
        let days = (e.Schedule.Week |> unwrapWeek)
        for day in days do
            totalHrs <- totalHrs + (day.Hours |> getHours)
        totalHrs

    // TODO:
    // - enable editing Schedule objects
    // - enable editing Day objects in arrays
    // - make generator function that can be used with different underlying types
    //    this would allow use for generating unique employee numbers and unique
    //    Position Ids
    // - enable editing employee priority
    // - functions for core logic
    //   - Create schedule
    //     - Make constraints
    //       - min 1 AD for each minute of open time at location
    //       - min 1 sensei for every 3 students scheduled at location for date
    //         - this should be configurable
    //       - Pick AD and sensei according to configurable params
    //         - Employee priority #
    //         - Desired weekly hours met or not
    //         - Schedule least hours possible for everyone
    //       - Close for day if scheduled students < 6 (1 sensei and 1 AD required)
    //   - Allow manual editing of schedule
    //     - Do not allow employee to be scheduled in same block of time at 2 locations
    //     - Do not allow employee to be scheduled in a block they aren't available

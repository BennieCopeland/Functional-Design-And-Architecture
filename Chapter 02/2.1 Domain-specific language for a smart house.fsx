///////////////////////
// Begin Listing 2.1 //
///////////////////////
type ThermometerName = string
type BarometerName = string

type Method =
    | ReadThermometer of ThermometerName
    | ReadBarometer of BarometerName
    | ReportTemperature
    | ReportAtmospherePressure
    | ClearData

type Language = Method list

let script =
    [
        ReadThermometer "Garage"
        ReadThermometer "Near the road"
        ReadThermometer "House"
        ReadBarometer "Garage"
        ReadBarometer "House"
        ReportTemperature
        ReportAtmospherePressure
        ClearData
    ]
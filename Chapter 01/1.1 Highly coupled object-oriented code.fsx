module Native =
    module Core =
        type Temperature =
            private
            | Kelvin of float
            | Celsius of float
        
        let (|Kelvin|Celsius|) (temperature: Temperature) =
            match temperature with
            | Kelvin v -> Kelvin v
            | Celsius v -> Celsius v
    
        let read (_: string) =
            Kelvin 0.0
        
        module Thermometer =
            let getData () =
                Kelvin 0.0

module Server =
    module Connection =
        let send (_, _, _) = ()

///////////////////////
// Begin Listing 1.1 //
///////////////////////
type Observer =
    member this.readAndSendTemperature () =
        let toCelsius (data: Native.Core.Temperature) : float =
            match data with
            | Native.Core.Kelvin v -> 273.15 - v
            | Native.Core.Celsius v -> v
        
        let received = Native.Core.Thermometer.getData()
        let inCelsius = toCelsius received
        let corrected = inCelsius - 12.5
        Server.Connection.send("temperature", "T-201A", corrected)
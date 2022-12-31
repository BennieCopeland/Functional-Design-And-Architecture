// I had to guess at the implementation of the two opened modules
// based upon their usage. I'm assuming the reason there is an
// Active Pattern for Kelvin and Celsius is to hide the implementation
// of Temperature. Additionally, F# is not as strict as Haskell and so
// I decided not to try and model the IO monad here.

module Native =
    module Core =
        module Thermometer =
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

module ServerContext =
    module Connection =
        let send _ _ _ = ()

// Listing 1.3
// F# doesn't have an equivalent of 'import qualified as'
open Native.Core
open ServerContext

type ReadThermometer = string -> Thermometer.Temperature
type SendTemperature = string -> float -> unit

let readThermometer : ReadThermometer =
    fun name ->
        Thermometer.read name

let sendTemperature : SendTemperature =
    fun name t ->
        Connection.send "temperature" name t
    
let readTemperature =
    let t1 = readThermometer "T-201A"
    
    match t1 with
    | Thermometer.Kelvin v -> 273.15 - v
    | Thermometer.Celsius v -> v

let readAndSend =
    let t1 = readTemperature
    let t2 = t1 - 12.5
    sendTemperature "T-201A" t2
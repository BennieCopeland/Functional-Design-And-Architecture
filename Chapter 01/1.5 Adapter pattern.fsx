module Native =
    module Core =
        module HighAccuracyThermometer =
            type Temperature =
                private
                | Kelvin of float
                | Celsius of float
            
            let (|Kelvin|Celsius|) (temperature: Temperature) =
                match temperature with
                | Kelvin v -> Kelvin v
                | Celsius v -> Celsius v
            
            let getData () =
                Kelvin 0.0
        
        module Utils =
            let toCelsius (data) : float =
                match data with
                | HighAccuracyThermometer.Kelvin v -> 273.15 - v
                | HighAccuracyThermometer.Celsius v -> v

type ISensor =
    abstract member GetData : unit -> float
    abstract member GetName : unit -> string
    abstract member GetDataType : unit -> string

///////////////////////
// Begin Listing 1.5 //
///////////////////////
[<Sealed>]
type HighAccuracyThermometer =
    member this.Name() = "HAT-53-2"
    member this.GetKelvin() =
        Native.Core.HighAccuracyThermometer.getData()

[<Sealed>]
type HAThermometerAdapter (thermometer: HighAccuracyThermometer) =
    let t = thermometer
    
    interface ISensor with
        member this.GetData () =
            let data = t.GetKelvin()
            Native.Core.Utils.toCelsius data
        member this.GetName () = t.Name()
        member this.GetDataType () = "temperature"
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

type ISensor =
    abstract member GetData : unit -> float
    abstract member GetName : unit -> string
    abstract member GetDataType : unit -> string

type IConnection =
    abstract member Send : string * string * float -> unit

[<Sealed>]
type Observer(sensor: ISensor, connection: IConnection) =
    member this.ReadAndSendData () =
        let data = sensor.GetData()
        let sensorName = sensor.GetName()
        let dataType = sensor.GetDataType()
        connection.Send(sensorName, dataType, data)

let toCelsius (data: Native.Core.Temperature) : float =
    match data with
    | Native.Core.Kelvin v -> 273.15 - v
    | Native.Core.Celsius v -> v

///////////////////////
// Begin Listing 1.4 //
///////////////////////
[<Sealed>]
type Receiver() =
    interface IConnection with
        member this.Send(name: string, dataType: string, v: float) =
            Server.Connection.send (name, dataType, v)

[<Sealed>]
type Thermometer() =
    let correction = -12.5
    
    let transform(data: Native.Core.Temperature) : float =
        (toCelsius data) + correction
        
    interface ISensor with
        member this.GetName() = "T-201A"
        member this.GetDataType() = "temperature"
        member this.GetData() =
            let data = Native.Core.Thermometer.getData()
            transform data

type Worker =
    member this.ObserveThermometerData() =
        let t = new Thermometer()
        let r = new Receiver()
        let observer = new Observer(t, r)
        observer.ReadAndSendData()
///////////////////////
// Begin Listing 1.2 //
///////////////////////
type ISensor =
    abstract member GetData : unit -> float
    abstract member GetName : unit -> string
    abstract member GetDataType : unit -> string

type IConnection =
    abstract member Send : (string * string * float) -> unit

[<Sealed>]
type Observer(sensor: ISensor, connection: IConnection) =
    member this.ReadAndSendData () =
        let data = sensor.GetData()
        let sensorName = sensor.GetName()
        let dataType = sensor.GetDataType()
        connection.Send(sensorName, dataType, data)
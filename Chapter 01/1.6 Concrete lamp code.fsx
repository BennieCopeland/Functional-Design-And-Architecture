type ILampSwitcher =
    abstract member Switch: bool -> unit

///////////////////////
// Begin Listing 1.6 //
///////////////////////
type DaylightLamp (n: string, v: int, onOff: bool) =
    let mutable isOn = onOff
    let mutable value = v
    let name = n
    
    interface ILampSwitcher with
        member this.Switch onOff =
            isOn <- onOff

type TableLamp (n: string, onOff: bool) =
    let mutable isOn = onOff
    let name = n
    
    interface ILampSwitcher with
        member this.Switch onOff =
            isOn <- onOff
            // Debug: will remove it later!
            failwith "Switched"
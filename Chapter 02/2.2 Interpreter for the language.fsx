// I decided to finally model the IO monad because of the List traversal
// part of the implementation.
type IO<'t> = IO of 't

[<AutoOpen>]
module IO =
    let retn v = IO v
    
    let bind (f : 'a->IO<'b>) (IO x) = f x
    
    let apply (IO f) (IO x) = IO (f x)
    
    type IOBuilder() =
        member this.Bind(x, f) = bind f x

        member this.Return(x) = retn x
        
        member this.ReturnFrom(x) = x
    
    let io = new IOBuilder()

module Smart =
    module Sensors =
        module Thermometer =
            type ThermometerName = string
            
            let lookup _ =
                Some ()
            
            let read _ =
                IO 0.0
        
        module Barometers =
            type BarometerName = string
            
            let lookup _ =
                Some ()
            
            let read _ =
                IO 0.0

type Method =
    | ReadThermometer of Smart.Sensors.Thermometer.ThermometerName
    | ReadBarometer of Smart.Sensors.Barometers.BarometerName
    | ReportTemperature
    | ReportAtmospherePressure
    | ClearData

type Language = Method list

module List =
    let traverseIO f list =
        let (<*>) = IO.apply
        let retn = IO.retn
        
        let cons head tail = head :: tail
        
        let initState = retn []
        let folder head tail = retn cons <*> (f head) <*> tail
        
        List.foldBack folder list initState

///////////////////////
// Begin Listing 2.2 //
///////////////////////
module Listing =
    open Smart.Sensors

    type Measurement =
        | TemperatureCelsius of Thermometer.ThermometerName * float
        | PressureAtmUnits of Barometers.BarometerName * float

    let isTemperature (_ : Measurement) : bool = true
    let isPressure (_ : Measurement) : bool = true
    let reportTemperature (_ : Measurement) : IO<unit> = IO ()
    let reportPressure (_ : Measurement) : IO<unit> = IO ()

    type Interpret = Measurement list -> Language -> IO<unit>
    let rec interpret : Interpret =
        fun ms lang ->
            match lang with
            | ReadThermometer name :: acts ->
                let mbTherm = Thermometer.lookup name
                match mbTherm with
                | Some therm ->
                    io {
                        let! value = Thermometer.read therm
                        let measurement = TemperatureCelsius (name, value)
                        return! interpret (measurement :: ms) acts
                    }
                | None ->
                    failwith "Thermometer not found"
            | ReadBarometer name :: acts ->
                let mbBar = Barometers.lookup name
                match mbBar with
                | Some bar ->
                    io {
                        let! value = Barometers.read bar
                        let measurement = PressureAtmUnits (name, value)
                        return! interpret (measurement :: ms) acts
                    }
                | None ->
                    failwith "Barometer not found"
            | ReportTemperature :: acts ->
                io {
                    let! _ = List.traverseIO reportTemperature ms
                    let ms' = List.filter (not << isTemperature) ms
                    return! interpret ms' acts
                }
            | ReportAtmospherePressure :: acts ->
                io {
                    let! _ = List.traverseIO reportPressure ms
                    let ms' = List.filter (not << isPressure) ms
                    return! interpret ms' acts
                }
            | ClearData :: acts ->
                interpret [] acts
            | [] ->
                IO ()

    type Interpreter = Language -> IO<unit>
    let interpreter : Interpreter =
        fun acts ->
            interpret [] acts

////////////////////////
// Supporting logging //
////////////////////////
module Logging =
    open Smart.Sensors

    type Measurement =
        | TemperatureCelsius of Thermometer.ThermometerName * float
        | PressureAtmUnits of Barometers.BarometerName * float

    let isTemperature (_ : Measurement) : bool = true
    let isPressure (_ : Measurement) : bool = true
    let reportTemperature (_ : Measurement) : IO<unit> = IO ()
    let reportPressure (_ : Measurement) : IO<unit> = IO ()
    let logStep _ = IO ()
    
    type Interpret = Measurement list -> Method -> IO<Measurement list>
    let interpret : Interpret =
        fun ms lang ->
            match lang with
            | ReadThermometer name ->
                let mbTherm = Thermometer.lookup name
                match mbTherm with
                | Some therm ->
                    io {
                        let! value = Thermometer.read therm
                        let measurement = TemperatureCelsius (name, value)
                        return (measurement :: ms)
                    }
                | None ->
                    failwith "Thermometer not found"
            | ReadBarometer name ->
                let mbBar = Barometers.lookup name
                match mbBar with
                | Some bar ->
                    io {
                        let! value = Barometers.read bar
                        let measurement = PressureAtmUnits (name, value)
                        return (measurement :: ms)
                    }
                | None ->
                    failwith "Barometer not found"
            | ReportTemperature ->
                io {
                    let! _ = List.traverseIO reportTemperature ms
                    let ms' = List.filter (not << isTemperature) ms
                    return ms'
                }
            | ReportAtmospherePressure ->
                io {
                    let! _ = List.traverseIO reportPressure ms
                    let ms' = List.filter (not << isPressure) ms
                    return ms'
                }
            | ClearData ->
                IO.retn []
    
    type Interpreter' = Measurement list -> Language -> IO<unit>
    let rec interpreter' : Interpreter' =
        fun ms lang ->
            io {
                match lang with
                | act :: acts ->
                    let! ms' = interpret ms act
                    
                    do! logStep act
                    
                    return! interpreter' ms' acts
                | [] ->
                    return ()
            }
    
    type Interpreter = Language -> IO<unit>
    let interpreter : Interpreter =
        fun acts ->
            interpreter' [] acts
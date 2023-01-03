///////////////////////
// Begin Listing 3.5 //
///////////////////////
module Andromeda =
    module Hardware =
        module Common =
            // Every physical instance of a component has its own GUID
            type PhysicalGuid = string

            // Components are grouped by name,
            // for example, temperature sensors AAA-T-25
            type ComponentName = string

            type Parameter      = Temperature | Pressure
            type ComponentClass = Sensors | Controllers
            type Vendor         = string

            type ComponentPassport = {
                ComponentClass : ComponentClass
                ComponentName : ComponentName
                ComponentGuid : PhysicalGuid
                ComponentVendor : Vendor
            }

    module Hdl =
        open Hardware.Common
    
        type ComponentIndex = string
        
        type ComponentDef =
            | Sensor of (ComponentPassport * ComponentIndex * Parameter)
            | Controller of (ComponentPassport * ComponentIndex)
        
        type Hdl = ComponentDef list
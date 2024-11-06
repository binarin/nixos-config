let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v23.0.0/Prelude/package.dhall
        sha256:397ef8d5cf55e576eab4359898f61a4e50058982aaace86268c62418d3027871

let Network
    : Type
    = { prefix : Natural
      , network : Text
      , gateway : Optional Text
      , dns : Optional (List Text)
      , domain : Optional Text
      }

let DeviceInterface
    : Type
    = { network : Network, address : Text, mac : Optional Text }

let DeviceConfig
    : Type
    = { interfaces : Prelude.Map.Type Text DeviceInterface }


let homeNetwork
    = { prefix = 24
      , network = "192.168.2.0"
      , gateway = Some "192.168.2.1"
      , dns = Some [ "192.168.2.46", "192.168.2.53" ]
      , domain = Some "home.binarin.info"
      } : Network

let smbSketchupNetwork
    = { prefix = 24
      , network = "172.16.242.0"
      , gateway = None Text
      , dns = None (List Text)
      , domain = None Text
      } : Network

let iface : Text -> DeviceInterface -> Prelude.Map.Entry Text DeviceInterface =
    \(name: Text) ->
    \(cfg: DeviceInterface) ->
    { mapKey = name, mapValue = cfg }

let Interfaces : Type = Prelude.Map.Type Text DeviceInterface

let host : Text -> DeviceConfig -> Prelude.Map.Entry Text DeviceConfig =
    \(name: Text) ->
    \(cfg: DeviceConfig) ->
    { mapKey = name, mapValue = cfg }


let hostStub : Text -> Prelude.Map.Entry Text DeviceConfig =
    \(name: Text) -> host name { interfaces = [] : Interfaces }

let allHosts : Prelude.Map.Type Text DeviceConfig =
    [ host "forgejo" { interfaces = [ iface "eth0" { network = homeNetwork, address = "192.168.2.3", mac = Some "BC:24:11:D9:78:49" } ] }
    , host "valak" { interfaces =
                         [ iface "br0" { network = homeNetwork, address = "192.168.2.26", mac = None Text }
                         , iface "smb-sketchup" { network = smbSketchupNetwork, address = "172.16.242.2", mac = None Text }
                         ]
                   }
    , hostStub "furfur"
    ]

in allHosts

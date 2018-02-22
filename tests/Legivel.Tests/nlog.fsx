module NlogInit

#I __SOURCE_DIRECTORY__
#I ".." 
#I "../../packages" 
#r @"test/NLog/lib/net45/NLog.dll"

open NLog
open System.IO

let With sd sf =
    //  Configure Nlog, logfile can be found under: ./src/TestScripts/logs/<scriptname>.log
    let nlogPath = Path.GetFullPath(Path.Combine(sd, "./nlog.config"))
    let logfile = Path.GetFullPath(Path.Combine(sd, "logs", (sprintf "%s.log" sf)))
    let xmlConfig = new NLog.Config.XmlLoggingConfiguration(nlogPath)
    xmlConfig.Variables.Item("logpath") <- Layouts.SimpleLayout(logfile)
    LogManager.Configuration <- xmlConfig


#### 0.3.0 - Januari 25 2019
*   Added netstandard2.0 support
*   Various performance improvements
*   Got all unit-tests workhng for all target-platforms
*   Upgraded to FAKE5, with latest Project Scaffold
*   Changed from Paket.Pack to DotNet.Pack

#### 0.2.2 - November 25 2018
*   Legivel.Parser: Performance impovements - Further tweaking
*   Feature: Processing options (Yaml.Mapper), cross check yaml with model (records)
*   Fix the creation of documentation

#### 0.2.1 - August 19 2018
*   Legivel.Parser: Performance impovments - complete rewrite 

#### 0.1.1 - November 26 2017
*   Legvel.Mapper: Added support for recursive types
*   Legivel.Parser: Made RegexDSL public
*   Legivel.Parser: Added CloneWith methods to GlobalTag
*   Legivel.Parser: Made GlobalTag.Create public
*   Legivel.Parser: Made GlobalTag.TagFunctions public
*   Legivel.Parser: Made NodeData.Create public
*   Legivel.Parser: Made ParseInfo.Create public
*   Legivel.Parser: Created extra SchemaUtils convenience functions for customization
*   Legivel.Parser: Moved MessageAtLine from Internals to Common namespace and made public
*   Legvel.Mapper: Made SuccessInfo.Create public
*   Legvel.Mapper: Made ErrorInfo.Create public


#### 0.1.0 - November 11 2017
* Legivel.Mapper: Added support for FSharp Map type
* Updated documentation for Map type


#### 0.0.8 - November 10 2017
* Fix: using YamlCore schema for int and float, as "20:30:56" was translated to a numeric
* Added unittest to check correct docs/tutorial output


#### 0.0.7 - November 08 2017
* Legivel.Mapper: Made "string" detection injectable (for record fieldnames)
* Legivel.Mapper: Fixed bug in strig to float parsing
* Legivel.Mapper: Added Timestamp primitive type
* Legivel.Mapper: Added Merge tag


#### 0.0.6 - October 28 2017
* Legivel.Mapper: map "null" for option types to "None"
* Legivel.Mapper: Made "null" detection generic/injectable
* Legivel.Mapper: removed field "StopLocation" in Succes result
* Added documentation for Legivel customization
* Added wishlist to documentation


#### 0.0.5 - October 25 2017
* Fixing too many framework dependencies issue
* Having another shot at nuget release


#### 0.0.4 - October 24 2017
* Fixing Nuget dependencies
* Adding more info to Nuge package


#### 0.0.3 - October 23 2017
* First Nuget package


#### 0.0.2 - October 22 2017
* Renamed product to 'Legivel'
* Wrote majority of the documentation


#### 0.0.1 - December 29 2016
* Initial source publication on Github (started in sept 2016)


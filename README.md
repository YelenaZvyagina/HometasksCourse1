# Hometasks

Здесь будут появляться домашние задания по программированию, возможно даже иногда правильные

## Builds


GitHub Actions |
:---: |
[![GitHub Actions](https://github.com/YelenaZvyagina/MyTask/workflows/Build%20master/badge.svg)](https://github.com/YelenaZvyagina/MyTask/actions?query=branch%3Amaster) |
[![Build History](https://buildstats.info/github/chart/YelenaZvyagina/MyTask)](https://github.com/YelenaZvyagina/MyTask/actions?query=branch%3Amaster) |

## NuGet

Package | Stable | Prerelease
--- | --- | ---
MyTask | [![NuGet Badge](https://buildstats.info/nuget/MyTask)](https://www.nuget.org/packages/MyTask/) | [![NuGet Badge](https://buildstats.info/nuget/MyTask?includePreReleases=true)](https://www.nuget.org/packages/MyTask/)


---

### Developing

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

or

- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)


---


### Build Targets


- `Clean` - Cleans artifact and temp directories.
- `DotnetRestore` - Runs [dotnet restore](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-restore?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- [`DotnetBuild`](#Building) - Runs [dotnet build](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `DotnetTest` - Runs [dotnet test](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-test?tabs=netcore21) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019.).
- `GenerateCoverageReport` - Code coverage is run during `DotnetTest` and this generates a report via [ReportGenerator](https://github.com/danielpalme/ReportGenerator).
- `WatchApp` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) on the application. Useful for rapid feedback loops.
- `WatchTests` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) with the test projects. Useful for rapid feedback loops.
- `GenerateAssemblyInfo` - Generates [AssemblyInfo](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.applicationservices.assemblyinfo?view=netframework-4.8) for libraries.
- `CreatePackages` - Runs the packaging task from [dotnet-packaging](https://github.com/qmfrederik/dotnet-packaging). This creates applications for `win-x64`, `osx-x64` and `linux-x64` - [Runtime Identifiers](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog).  
    - Bundles the `win-x64` application in a .zip file.
    - Bundles the `osx-x64` application in a .tar.gz file.
    - Bundles the `linux-x64` application in a .tar.gz file.
- `GitRelease` - Creates a commit message with the [Release Notes](https://fake.build/apidocs/v5/fake-core-releasenotes.html) and a git tag via the version in the `Release Notes`.
- `GitHubRelease` - Publishes a [GitHub Release](https://help.github.com/en/articles/creating-releases) with the Release Notes and any NuGet packages.
- `FormatCode` - Runs [Fantomas](https://github.com/fsprojects/fantomas) on the solution file.
- [`Release`](#Releasing) - Task that runs all release type tasks such as `GitRelease` and `GitHubRelease`. Make sure to read [Releasing](#Releasing) to setup your environment correctly for releases.

---


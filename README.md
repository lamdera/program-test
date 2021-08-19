# program-test

Write tests that let you simulate a user interacting with your Lamdera app.

## Getting started

In order for this package to work, all `Cmd`s and `Sub`s need to be replaced with `Command` and `Subscription`.

Additionally, the following modules
```
Browser
Browser.Dom
Browser.Events
Browser.Navigation
File
File.Download
File.Select
Http
Lamdera
Process
Task
Time
```
need to be replaced with
```
Effect.Browser
Effect.Browser.Dom
Effect.Browser.Events
Effect.Browser.Navigation
Effect.File
Effect.File.Download
Effect.File.Select
Effect.Http
Effect.Lamdera
Effect.Process
Effect.Task
Effect.Time
```
anywhere they are imported or used. A consequence of this is that you'll need to vendor any packages that use these modules in order to fix their imports.

Note that while the `Effect.*` version of these modules tries to match the API of the original module in most cases, sometimes that's not possible, and in a few places, they differ intentionally in order to provide a better user experience.  
For example, `Process.sleep` takes a `Float` and you have to read the docs to know that's in milliseconds. `Effect.Process.sleep` instead uses [`Duration`](
) which lets you choose if you want to use milliseconds, seconds, minutes, etc.
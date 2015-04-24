# Lager Airbrake

This is a [Lager](https://github.com/basho/lager) backend for [Airbrake](https://www.airbrake.io). If you are using Lager in your application, this backend will report the errors with the desired level of severity to Airbrake.

Lager Airbrake will try to use some heuristics to optimize the notification sent to Airbrake, so that errors can be easily differentiated.


### Install

In your project's `rebar.config` file, add `lager_airbrake` as a dependency:

```erlang
{lager_airbrake, ".*", {git, "https://github.com/ostinelli/lager_airbrake.git", "master"}}
```

Then include `lager_airbrake_backend` in your `lager` configuration:

```erlang
%% Lager config
{lager, [
    {handlers, [
        %% Add Airbrake monitoring
        {lager_airbrake_backend, [
            {environment, "development"},
            {api_key, "AIRBRAKE_API_KEY"},
            {project_id, "AIRBRAKE_PROJECT_ID"},
            {level, warning}
        ]},

        [...]
    ]},

[...]
}
```

### Dependencies
Dependencies are automatically managed by [rebar](https://github.com/rebar/rebar). These are:

 * [Hackney](https://github.com/benoitc/hackney) - HTTP client
 * [Jiffy](https://github.com/davisp/jiffy) - JSON encoder

### Contributing
So you want to contribute? That's great!

Every pull request should have its own topic branch. In this way, every additional adjustments to the original pull request might be done easily, and squashed with `git rebase -i`. The updated branch will be visible in the same pull request, so there will be no need to open new pull requests when there are changes to be applied.

Do not commit to master in your fork. Provide a clean branch without merge commits.


### Legal notice
All product names and trademarks are the property of their respective owners, which are in no way associated or affiliated with the developer.

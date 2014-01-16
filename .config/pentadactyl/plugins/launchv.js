"use strict";
var INFO =
["plugin", { name: "launchv",
             href: "https://github.com/holomorph/dotfiles",
             summary: "Launch videos",
             xmlns: "dactyl" },
    ["license", { href: "http://people.freebsd.org/~phk/" }, "BEER-WARE"],
    ["project", { name: "Pentadactyl", "min-version": "1.0" }],

    ["p", {},
    "This plugin helps you launch videos using an external player. ",
    "The following tools are used:"],

    ["ul", {},
    ["li", {}, "livestreamer, a CLI program that extracts stream info."],
    ["li", {}, "mpv, a movie player based on MPlayer and mplayer2."],
    ["li", {}, "quvi, a CLI tool for parsing media stream properties."]],

    ["p", {},
    "A player is started in a shell using a buffer or hint URL. ",
    "Commands fed to the shell are sanitized to prevent injection. ",
    "An ex command and hint mode are provided."],

    ["item", {}, ["tags", {}, ":launchv"],
    ["spec", {}, ["ex", {}, ":launchv"]],
    ["description", { short: "true" },
    ["p", {}, "Launch video in the current buffer."]]],

    ["item", {}, ["tags", {}, ";l"],
    ["spec", {}, ["hints", {}, ";l"]],
    ["description", { short: "true" },
    ["p", {}, "Launch video using hint URL."]]],

    ["note", {}, "User configuration is required for the plugin to work. ",
    "For example, ", ["em", {}, "lstream"], " might be:",
    ["code", {},
    "#!/bin/bash\n",
    "exec livestreamer -pmpv \"$1\" \"${2:-best,mobile_source}\""]]

    ];

function launchv(target) {
    var uri = target.replace(/([$`"\\])/g, "\\$1");

    function exec(launcher, uri) {
        return commands.execute(launcher + ' "' + uri + '" &');
    }

    /* filter certain urls to more appropriate programs before passing to
     * quvi */
    if(uri.match(/twitch\.tv/))
        exec("!lstream", uri);
    else if(uri.match(/playlist\?list=PL/))
        exec("!mpv --really-quiet --cache=4096", uri );
    else
        exec("!quvi dump -b mute", uri);
}

hints.addMode("l", "Launch video from hint", function (elem, loc) launchv(loc));

group.commands.add(["launchv", "lv"], "Launches current buffer video.",
    function(args) { launchv(buffer.URL); });

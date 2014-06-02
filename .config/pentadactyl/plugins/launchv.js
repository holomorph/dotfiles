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
    ["li", {}, "quvi, a CLI tool for parsing media stream properties."],
    ["li", {}, "youtube-dl, a CLI tool to download videos from various media sites."]],

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
    /* Escape anything which could be used to inject shell commands before
     * passing it to the commands */
    var uri = target.replace(/([$`"\\])/g, "\\$1");

    function exec(launcher, uri) {
        if(typeof dactyl !== "undefined")
            dactyl.echomsg("Launch " + launcher + " " + uri);
        return io.system(launcher + ' "' + uri + '" &');
    }

    /* filter certain urls to more appropriate programs before passing to
     * quvi */
    if(uri.match(/twitch\.tv\/.*\/c\/[0-9]+/))
        exec("yt-dl", uri);
    else if(uri.match(/twitch\.tv/))
        exec("lstream", uri);
    else if(uri.match(/youtube.*[?&]list=PL/)) {
        /* Check if the url is part of a playlist but a direct video
         * (watch?v=) url is provided and return the real playlist url */
        if(uri.match(/watch\?v=/))
            exec("mpv --no-terminal", uri.replace(/watch\?v.+?\&/, "playlist\?"));
        else
            exec("mpv --no-terminal", uri);
    }
    else
        exec("yt-dl", uri);
}

hints.addMode("l", "Launch video from hint", function (elem, loc) launchv(loc));

commands.add(["launchv", "lv"], "Launches current buffer video.",
    function(args) { launchv(buffer.URL); });

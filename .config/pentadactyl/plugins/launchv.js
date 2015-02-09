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
    ["li", {}, "mpv, a media player based on MPlayer and mplayer2."],
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

    ["note", {}, "youtube-dl support was added to mpv in v0.7.0."],
    ["note", {}, "livestreamer needs to have a player and default stream ",
    "configured. livestreamer added the --default-stream option in v1.9.0."]

    ];

function launchv(target) {
    /* Escape anything which could be used to inject shell commands before
     * passing it to the commands */
    var uri = target.replace(/([$`"\\])/g, "\\$1");

    function exec(launcher, uri) {
        if(!uri || uri.length === 0) {
            if(typeof dactyl !== "undefined" && "echoerr" in dactyl)
                dactyl.echoerr("E474: Invalid argument");
            else if(typeof liberator !== "undefined" && "echoerr" in liberator)
                liberator.echoerr("E474: Invalid argument");
            return;
        }
        if(typeof dactyl !== "undefined")
            dactyl.echomsg("Launch " + launcher + " " + uri);
        io.system(launcher + ' "' + uri + '" &');
    }

    /* filter certain urls to more appropriate programs before passing to
     * mpv */
    if(uri.match(/(hitbox|twitch)\.tv/))
        exec("livestreamer", uri);
    else
        exec("mpv --ytdl", uri);
}

hints.addMode("l", "Launch video from hint", function (elem, loc) launchv(loc));

commands.add(["launchv", "lv"], "Launches current buffer video.",
    function(args) { launchv(buffer.URL); });

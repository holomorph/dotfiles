function launchv(target) {
    var uri = target.replace(/([$`"\\])/g, "\\$1");
    var cmd;

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

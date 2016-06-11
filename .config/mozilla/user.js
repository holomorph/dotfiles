user_pref("extensions.pocket.enabled", false);
user_pref("browser.cache.disk.enable", false);
user_pref("browser.newtabpage.enhanced", false);
user_pref("browser.tabs.onTop", false);
user_pref("browser.warnOnQuit", false);
user_pref("general.smoothScroll", false);
user_pref("gfx.color_management.enablev4", true);
user_pref("loop.enabled", false);
user_pref("plugin.default.state", 1);
user_pref("plugins.click_to_play", true);
user_pref("plugins.hide_infobar_for_missing_plugin", true);
user_pref("privacy.trackingprotection.enabled", true);
user_pref("signon.rememberSignons", false);

// hardening
user_pref("beacon.enabled", false);
user_pref("browser.send_pings", false);
user_pref("device.sensors.enabled", false);
user_pref("dom.battery.enabled", false);
user_pref("geo.enabled", false);
user_pref("geo.wifi.uri", "");
user_pref("media.eme.enabled", false);
user_pref("media.peerconnection.enabled", false);
user_pref("network.http.sendRefererHeader", 0);
user_pref("network.http.referer.XOriginPolicy", 1);
user_pref("network.http.referer.spoofSource", true);
user_pref("network.http.referer.trimmingPolicy", 2);
user_pref("network.http.speculative-parallel-limit", 0);
user_pref("network.prefetch-next", false);
user_pref("network.proxy.socks_remote_dns", true);
user_pref("browser.safebrowsing.enabled", false);
user_pref("browser.safebrowsing.downloads.enabled", false);
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("security.ask_for_password", 0);
user_pref("security.cert_pinning.enforcement_level", 2);
user_pref("toolkit.telemetry.enabled", false);

// show unsafe SSL negotiation
user_pref("security.ssl.require_safe_negotiation", false);
user_pref("security.ssl.treat_unsafe_negotiation_as_broken", true);

// NoScript
user_pref("noscript.notify", false);
user_pref("noscript.showPlaceholder", false);

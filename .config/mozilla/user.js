user_pref("browser.tabs.onTop", false);
user_pref("general.smoothScroll", false);
user_pref("plugin.default.state", 1);
user_pref("plugins.click_to_play", true);
user_pref("plugins.hide_infobar_for_missing_plugin", true);
user_pref("signon.rememberSignons", false);

// hardening
user_pref("browser.send_pings", false);
user_pref("media.peerconnection.enabled", false);
user_pref("network.http.sendRefererHeader", 0);
user_pref("security.ask_for_password", 0);
user_pref("security.cert_pinning.enforcement_level", 2);

// show unsafe SSL negotiation
user_pref("security.ssl.require_safe_negotiation", false);
user_pref("security.ssl.treat_unsafe_negotiation_as_broken", true);

// NoScript
user_pref("noscript.notify", false);
user_pref("noscript.showPlaceholder", false);

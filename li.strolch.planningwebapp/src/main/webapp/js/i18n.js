var i18n = {
    resource: "messages",
    path: null,
    locale: null,
    bundle: null,

    detectLocale: function () {
        var locale;
        locale = navigator.language; // (Netscape - Browser Localization)
        if (!locale) {
            locale = navigator.browserLanguage; // (IE-Specific - Browser Localized Language)
        }
        if (!locale) {
            locale = navigator.systemLanguage; // (IE-Specific - Windows OS - Localized Language)
        }
        if (!locale) {
            locale = navigator.userLanguage;
        }
        if (!locale) {
            locale = "en";
            console.info("Defaulting to " + locale)
        }

        if (locale.length > 2) {
            console.info("Shortening locale to top level: " + locale)
            locale = locale.substring(0, 2);
        }

        console.info("Current locale: " + locale);
        return locale;
    },

    load18n: function (path, locale, resource) {

        var jsonGet = function (url) {
            var data = [null
            ];

            var client = new XMLHttpRequest();
            client.onreadystatechange = function () {
                if (client.readyState == 4) {
                    if (client.status == 200) {
                        try {
                            var queryResult = JSON.parse(client.responseText);
                            data[0] = queryResult;
                        } catch (e) {
                            alert("Failed to parse i18n from server: " + e);
                            data[0] = {};
                        }
                    }
                }
            };

            client.open("GET", url, false);
            client.setRequestHeader("Accept", "application/json");
            client.send();
            return data[0];
        };

        var locales = [];
        if (locale != null) {
            locales.push(locale);
            var sepIndex = locale.indexOf("-", 0);
            if (sepIndex == -1)
                sepIndex = locale.indexOf("_", 0);
            if (sepIndex != -1)
                locales.push(locale.substring(0, sepIndex));
        }
        if (locale.indexOf("en") != 0)
            locales.push("en");

        var bundle = null;
        for (i = 0; i < locales.length; ++i) {
            var loc = locales[i];
            var url = path + "/" + loc + "/" + resource + ".json?" + Math.random(); // prevent caching
            bundle = jsonGet(url);
            if (bundle != null) {
                i18n.locale = loc;
                break;
            }
        }

        return bundle;
    },

    init: function () {
        if (this.locale == null) {
            this.locale = this.detectLocale();
        }
        if (this.bundle == null) {
            this.bundle = this.load18n(this.path, this.locale, this.resource);
        }
    },

    t: function (key, properties) {
        this.init();
        var msg = this.bundle[key];
        if (msg == null)
            msg = key;

        if (properties) {
            $.each(properties, function (key, value) {
                msg = msg.replace('${' + key + '}', value);
            });
        }

        return msg;
    },

    translate_document: function () {
        var listToI18n = document.querySelectorAll("*[data-i18n]");
        // console.info("Translating " + listToI18n.length + " elements.")
        Array.prototype.slice.call(listToI18n).forEach(function (itemToI18n, index, arr) {
            var key = itemToI18n.getAttribute("data-i18n");
            if (key != null && key.length > 0) {
                var msg = i18n.t(key);
                if (msg == null || msg.length == 0) {
                    console.info("Missing translation for key: " + key);
                } else {
                    if (itemToI18n.localName == 'input') {
                        if (itemToI18n.type == 'submit' || itemToI18n.type == 'button') {
                            itemToI18n.value = msg;
                        } else {
                            itemToI18n.placeholder = msg;
                        }
                    } else if(itemToI18n.localName == 'button') {
                        itemToI18n.text = msg;
                    } else {
                        itemToI18n.textContent = msg;
                    }
                }
            }
        });
    }
};

/**
 * Created by eitch on 2015-09-04
 */

/*
 * Define the index namespace
 */
if (typeof strolch == 'undefined') {
    strolch = {};
}
strolch.index = {};


jQuery(document).ready(function ($) {
    strolch.index.init();
});

strolch.index.init = function () {

    strolch.fn.translateI18n();

    // logout if no session data
    if (strolch.fn.getAuthToken() == null || strolch.fn.getSessionData() == null) {
        console.log('AuthToken or session data unavailable, logging out...');
        strolch.fn.logout();
        return;
    }

    strolch.fn.multipleModalsHack();
    strolch.index.registerHandlers();

    // validate session still alive
    console.log('AuthToken exists, validating...');
    strolch.fn.validateAuthToken().complete(function (data) {
        if (data.status == 200) {
            console.log('AuthToken valid, starting...');
            strolch.index.start();
        } else {
            console.log('AuthToken not valid, user has to reauthenticate.');
            $('#auth-username').val(strolch.fn.getSessionData().username);
            strolch.fn.showReAuthForm();
        }
    });
};

strolch.index.registerHandlers = function () {

    strolch.fn.onModalShow('#reauthFormModal', strolch.fn.logout);
    strolch.fn.onModalFormSubmit('#reauthFormModal', '#reauthForm', function () {
        setTimeout(strolch.fn.reAuth, 3000);
    });
};

strolch.index.start = function () {

    var components = ['model'];
    var componentsLoaded = [];
    var componentsFailed = [];

    var handleLoadDone = function () {

        if (!strolch.fn.equalsArray(components, componentsLoaded)) {
            return;
        }

        if (componentsFailed.length != 0) {
            alertify.alert(i18n.t('component.load.error'), i18n.t('component.load.error.msg', {components: componentsFailed}));
            return;
        }

        console.log("Finished loading parts.");
    };

    $.each(components, function (index, value) {

        var component = value;

        $("#page-content").load("parts/" + component + ".html", function (responseText, textStatus, req) {

            if (req.status != 200) {

                console.error("Failed to load HTML for " + component + " due to status " + req.status);
                componentsFailed.push(component);
                componentsLoaded.push(component);
                handleLoadDone();

            } else {

                console.log("Loaded HTML for " + component);

                $.getScript("js/parts/" + component + ".js", function (responseText, textStatus, req) {
                    console.log("Loaded JS for " + component);

                    if (req.status != 200) {
                        console.error("Failed to load JS for " + component + " due to status " + req.status);
                        componentsFailed.push(component);
                    } else {

                        var componentNs = strolch.parts[component];
                        if (componentNs === 'undefined' || componentNs.init === 'undefined' || componentNs.show === 'undefined') {
                            componentsFailed.push(component + "(missing NS, init() or show())");
                        }

                        try {
                            componentNs.init();
                        } catch (e) {
                            console.error(e);
                            componentsFailed.push(component + "(init failed!)");
                        }
                    }

                    componentsLoaded.push(component);
                    handleLoadDone();
                });
            }
        });
    });
    
};


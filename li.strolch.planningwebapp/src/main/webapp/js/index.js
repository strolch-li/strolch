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

strolch.const.partNames = ['model'];

jQuery(document).ready(function ($) {
    strolch.index.init();
});

strolch.index.init = function () {

    strolch.fn.translateI18n();

    // set version
    $('#footer').find('.version').text('Version ' + strolch.fn.version());

    // logout if no session data
    if (strolch.fn.getAuthToken() == null || strolch.fn.getSessionData() == null) {
        console.log('AuthToken or session data unavailable, logging out...');
        strolch.fn.logout();
        return;
    }

    // multiple Modal hack
    strolch.fn.multipleModalsHack();

    // re-auth modal form handler
    strolch.fn.onModalShow('#reauthFormModal', strolch.fn.logout);
    strolch.fn.onModalFormSubmit('#reauthFormModal', '#reauthForm', function () {
        setTimeout(strolch.fn.reAuth, 3000);
    });

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


strolch.index.start = function () {

    strolch.fn.loadParts(strolch.const.partNames);
    //strolch.index.loadParts();
    strolch.index.registerHandlers();
};


strolch.index.registerHandlers = function () {

    strolch.index.registerNavigationHandlers();
};


strolch.index.registerNavigationHandlers = function () {

    $('*[data-function=navigate]').unbind('click').click(function (e) {

        var target = $(this).data('target');
        var part = strolch.parts[target];
        if (part === 'undefined') {
            alertify.alert(i18n.t('navigate.error'), i18n.t('navigate.error.missingpart', {part: part}));
            return;
        }

        $.each(strolch.const.partNames, function (index, value) {
            strolch.parts[value].hide();
        });
        part.show();
    });
};

strolch.index.loadParts = function () {
    var partsLoaded = [];
    var partsFailed = [];

    var handleLoadDone = function () {

        if (!strolch.fn.equalsArray(strolch.const.partNames, partsLoaded)) {
            return;
        }

        if (partsFailed.length != 0) {
            alertify.alert(i18n.t('part.load.error'), i18n.t('part.load.error.msg', {parts: partsFailed}));
            return;
        }

        console.log("Finished loading parts.");
    };

    $.each(strolch.const.partNames, function (index, value) {

        var partName = value;

        $("#page-content").load("parts/" + partName + ".html", function (responseText, textStatus, req) {

            if (req.status != 200) {

                console.error("Failed to load HTML for " + partName + " due to status " + req.status);
                partsFailed.push(partName);
                partsLoaded.push(partName);
                handleLoadDone();

            } else {

                console.log("Loaded HTML for " + partName);

                $.getScript("js/parts/" + partName + ".js", function (responseText, textStatus, req) {
                    console.log("Loaded JS for " + partName);

                    if (req.status != 200) {
                        console.error("Failed to load JS for " + partName + " due to status " + req.status);
                        partsFailed.push(partName);
                    } else {

                        var part = strolch.parts[partName];
                        if (part === 'undefined' || part.init === 'undefined' || part.show === 'undefined') {
                            partsFailed.push(partName + "(missing part, init() or show())");
                        }

                        try {
                            part.init();
                        } catch (e) {
                            strolch.fn.logException(e);
                            partsFailed.push(partName + "(init failed!)");
                        }
                    }

                    partsLoaded.push(partName);
                    handleLoadDone();
                });
            }
        });
    });
};

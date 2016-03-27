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

};
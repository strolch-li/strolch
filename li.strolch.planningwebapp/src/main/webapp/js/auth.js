/**
 * Created by eitch on 2015-09-04
 */

/*
 * Define the main namespace
 */
if (typeof strolch == 'undefined') {
    strolch = {};
}
strolch.auth = {};


jQuery(document).ready(function ($) {
    strolch.auth.init();
});

strolch.auth.init = function () {

    strolch.fn.translateI18n();

    var usernameTxt = $('#auth-username');
    var passwordTxt = $('#auth-password');

    $('#authForm').on('submit', strolch.auth.submitAuthForm);

    var authToken = strolch.fn.getAuthToken();
    var sessionData = strolch.fn.getSessionData();

    if (authToken && sessionData) {
        console.log('AuthToken exists, validating...');

        usernameTxt.prop('disabled', true);
        passwordTxt.prop('disabled', true);

        $('#info').removeClass('hidden').text(i18n.t('auth-validating-token'));
        strolch.fn.toggleSubmitBtn('#authForm');

        // delay a bit
        setTimeout(function () {
            strolch.fn.validateAuthToken().complete(function (data) {
                if (data.status == 200) {
                    var page = $('#authForm').data('submit-dst');
                    window.location.href = strolch.fn.url(page);
                    console.log('AuthToken valid, navigating to ' + page);
                } else {
                    console.log('AuthToken not valid, user has to login.');
                    $('#info').text(i18n.t('auth-token-invalid'));
                    usernameTxt.prop('disabled', false).val(strolch.fn.getSessionData().username);
                    passwordTxt.prop('disabled', false).focus();
                    strolch.fn.toggleSubmitBtn('#authForm');
                }
            });
        }, 3000);
    } else if (sessionData) {

        usernameTxt.prop('disabled', false).val(strolch.fn.getSessionData().username);
        passwordTxt.focus();

    }
};

strolch.auth.submitAuthForm = function (e) {

    var username = $('#auth-username').val();
    if (strolch.fn.isEmptyString(username) || username.length < 2) {
        alertify.alert(i18n.t('auth.warning'), i18n.t('auth.username.empty'));
        return;
    }

    var password = $('#auth-password').val();
    if (strolch.fn.isEmptyString(password) || password.length < 2) {
        alertify.alert(i18n.t('auth.warning'), i18n.t('auth.username.empty'));
        return;
    }

    strolch.fn.toggleSubmitBtn('#authForm');
    setTimeout(function () {
        strolch.fn.doAuth('#authForm', username, password);
    }, 3000);
};

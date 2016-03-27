/**
 * Created by eitch on 2015-09-04
 */


/*
 * Define the main namespace
 */
if (typeof strolch == 'undefined') {
    strolch = {};
}
strolch.fn = {};


/*
 * Helpers
 */
strolch.fn.url = function (subPath) {
    var path = strolch.const.url_base + '/' + subPath;
    return window.location.origin + '/' + path;
};

strolch.fn.translateI18n = function (locale) {
    if (locale == null || locale.length == 0 || locale == 'undefined') {
        console.log('Locale is \'' + locale + '\', detecting browser locale...');
        locale = i18n.detectLocale();
        console.log('Locale is now ' + locale);
    }

    i18n.path = 'localization';
    i18n.locale = locale;
    i18n.resource = 'localization';
    i18n.init(locale);
    i18n.translate_document();
    strolch.fn.setLocale(i18n.locale);
};

/*
 * configuration
 */
strolch.fn.getAuthToken = function () {
    return localStorage[strolch.const.auth_token];
};
strolch.fn.setAuthToken = function (authToken) {
    return localStorage[strolch.const.auth_token] = authToken;
};
strolch.fn.getSessionData = function () {
    var data = localStorage[strolch.const.session_data];
    if (data == null)
        return null;
    return JSON.parse(data);
};
strolch.fn.setSessionData = function (data) {
    if (data == null)
        throw "Data can not be null!";
    return localStorage[strolch.const.session_data] = JSON.stringify(data);
};
strolch.fn.clearSessionData = function () {
    localStorage.clear();
    sessionStorage.clear();
};

strolch.fn.getLocale = function () {
    return localStorage['language_locale'];
};
strolch.fn.setLocale = function (locale) {
    localStorage['language_locale'] = locale;
};

strolch.const.version = null;
strolch.fn.version = function () {

    if (strolch.const.version == null) {
        strolch.const.version = "unknown";
        $.ajax({
                async: false,
                url: strolch.fn.url(strolch.const.urls.version)
            }
        ).done(function (data) {
            if (data != null) {
                var ver = data['artifactVersion'];
                if (data['scmRevision'] == '${buildNumber}') {
                    strolch.const.version = ver;
                } else {
                    ver = ver ? ver.substr(0, 9) : '?';
                    var rev = data['scmRevision'];
                    var rev = rev ? rev.substr(0, 7) : '?';
                    strolch.const.version = ver + " - " + rev;
                }
            }
        });
    }
    return strolch.const.version;
};

strolch.const.revision = null;
strolch.fn.revision = function () {

    if (strolch.const.revision == null) {
        strolch.const.revision = Math.floor(Math.random() * 10000000);
        $.ajax({
                async: false,
                url: strolch.fn.url(strolch.const.urls.version)
            }
        ).done(function (data) {
            if (data != null && data['scmRevision'] != '${buildNumber}') {
                strolch.const.revision = data['scmRevision'];
            }
        });
    }
    return strolch.const.revision;
};


/*
 * Session management
 */
strolch.fn.showReAuthForm = function () {
    $('#reauthFormModal').on('shown.bs.modal', function () {
        $('#auth-password').focus();
    });
    $('#reauthFormModal').modal('show');
};
strolch.fn.reAuth = function () {

    var username = $('#auth-username').val();
    if (strolch.fn.isEmptyString(username)) {
        alertify.alert(i18n.t('auth.warning'), i18n.t('auth.username.empty'));
        return;
    }

    var password = $('#auth-password').val();
    if (strolch.fn.isEmptyString(password)) {
        alertify.alert(i18n.t('auth.warning'), i18n.t('auth.username.empty'));
        return;
    }

    console.log('Reauthenticating...');
    strolch.fn.doAuth('#reauthForm', username, password);
};

strolch.fn.doAuth = function (authFormSel, username, password) {

    var payload = JSON.stringify({
        username: username,
        password: btoa(password)
    });

    var url = strolch.fn.url(strolch.const.urls.auth);

    $.ajax({
        type: "POST",
        url: url,
        data: payload,
        contentType: 'application/json'
    }).success(function (data) {

        strolch.fn.toggleSubmitBtn(authFormSel);
        strolch.fn.setSessionData(data);
        strolch.fn.setAuthToken(data.authToken);
        console.log('Authenticated.');

        var authForm = $(authFormSel);
        var submitAction = authForm.data('submit-action');
        if (submitAction == 'navigate') {
            var navPage = $(authFormSel).data('submit-dst');
            var page = strolch.fn.url(navPage);
            console.log('User logged in, navigating to ' + page);
            window.location.href = page;
        } else if (submitAction == 'dismiss-modal') {
            var modalSel = $(authFormSel).data('submit-dst');
            strolch.fn.dismissModal(modalSel);
        } else if (submitAction == 'reload') {
            window.location.reload();
        } else {
            throw 'Unhandled submit-action ' + submitAction + ' on form ' + authFormSel;
        }


    }).fail(function (data) {

        strolch.fn.toggleSubmitBtn(authFormSel);

        if (data.status == 404) {
            alertify.alert(i18n.t('auth.failed'), i18n.t('auth.serverdown'));
        } else {
            var msg = data.responseJSON.msg;
            if (!msg)
                msg = data.statusText;
            alertify.alert(i18n.t('auth.failed'), data.status + ': ' + msg);
        }
    });
};

strolch.fn.logout = function () {

    var locale = strolch.fn.getLocale();
    strolch.fn.clearSessionData();
    strolch.fn.setLocale(locale);

    var url = strolch.fn.url('auth.html');
    console.log('Logging out and redirecting to ' + url);
    window.location.href = url;
};

strolch.fn.validateAuthToken = function () {

    var url = strolch.fn.url(strolch.const.urls.auth) + '/' + strolch.fn.getAuthToken();

    return $.ajax({
        type: "HEAD",
        url: url
    });
};


/*
 * Modal panel helpers
 */
strolch.fn.onModalShow = function (modalSelector, hideCallback) {

    var modal = $(modalSelector);
    modal.on('show.bs.modal', function (e) {
        modal.on('hide.bs.modal', function (e) {
            setTimeout(hideCallback, 5);
        });
        modal.find('button[data-dismiss=modal]').prop("disabled", false);
        modal.find('button[type=submit]').prop("disabled", false);
        modal.find('span[data-type=spinner]').hide();
    });
};

strolch.fn.onModalFormSubmit = function (modalSelector, formId, submitCallback) {
    var modal = $(modalSelector);
    $(formId).on('submit', function (e) {
        modal.unbind('hide.bs.modal').on('hide.bs.modal', function (e) {
            return false;
        });
        modal.find('button[data-dismiss=modal]').prop("disabled", true);
        modal.find('button[type=submit]').prop("disabled", true);
        modal.find('span[data-type=spinner]').show();

        setTimeout(submitCallback, 5);
    });
};

strolch.fn.dismissModal = function (modalSelector) {
    console.log('Dismissing modal ' + modalSelector);
    var modal = $(modalSelector);
    modal.unbind('hide.bs.modal').on('hide.bs.modal', function (e) {
        return true;
    });
    modal.modal('hide');
};

strolch.fn.toggleSubmitBtn = function (formId) {

    var form = $(formId);

    var submitBtn = form.find('button[type=submit]');
    submitBtn.prop("disabled", !submitBtn.prop("disabled"));

    var spinner = form.find('span[data-type=spinner]');
    if (spinner.is(':visible'))
        spinner.hide();
    else
        spinner.show();
};

/*
 * Part loading
 */
strolch.fn.loadPart = function (part, domParent) {

    var revision = '_=' + strolch.fn.revision();

    var templatePath = localStorage['path_template'];
    if (typeof templatePath == 'undefined' || templatePath == '') {
        alertify.error('Template path configuration missing!');
        return;
    }
    var templateUrl = templatePath + page + '.html?' + revision;

    var scriptPath = localStorage['path_script'];
    if (typeof scriptPath == 'undefined' || scriptPath == '') {
        alertify.error('scriptPath path configuration missing!');
        return;
    }
    var scriptUrl = scriptPath + 'templates/' + page + '.js?' + revision;

    $('.content-loading, .loading-background').showLoading(true);
    var container = $(this);
    container.empty();
    container.hide();

    var css_url = 'css/templates/' + page + '.css?' + revision;

    this.load(templateUrl + ' ' + selection, function (response, status, xhr) {
        if (status === 'success') {

            if ($('head link[data-page=' + page + ']').length == 0)
                $('head').append($('<link rel="stylesheet" type="text/css" />').attr('data-page', page).attr('href', css_url));

            $.getScript(scriptUrl)
                .done(function (script, textStatus) {
                    initScript(page);
                    i18n.translate_document();
                    container.show();
                    $('.content-loading, .loading-background').showLoading(false);
                })
                .fail(function (jqxhr, settings, exception) {

                    var title;
                    var msg;
                    if (jqxhr.status == 404) {
                        title = 'Missing script';
                        msg = 'Server says script does not exist at URL ' + scriptUrl;
                    } else {
                        title = 'Get script failed!';
                        msg = exception.message;
                    }
                    alertify.alert(title, msg);
                    console.error(title + '\n:' + msg);
                    if (exception.stack)
                        console.error(exception.stack);
                });

        } else {

            if (xhr.status == 404)
                alertify.alert('Missing template', 'Server says template does not exist at URL ' + templateUrl);
            else
                alertify.alert('Failed to load template', 'Failed to load template at url ' + templateUrl);

            container.show();
            $('.content-loading, .loading-background').showLoading(false);

            console.error('error: ' + status);
            console.error(response);
        }
    });
};


/*
 * Utils
 */
strolch.fn.uuid = function () {
    var d = new Date().getTime();
    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
        var r = (d + Math.random() * 16) % 16 | 0;
        d = Math.floor(d / 16);
        return (c == 'x' ? r : (r & 0x3 | 0x8)).toString(16);
    });
    return uuid;
};

strolch.fn.emptyString = function (val) {
    return strolch.fn.isEmptyString(val);
};

strolch.fn.isEmptyString = function (val) {
    return typeof val == 'undefined' || val == '';
};

strolch.fn.isFloat = function (val) {
    return Number(parseFloat(val)) == val;
};

strolch.fn.isInteger = function (val) {
    return Number(parseInt(val)) == val;
};

strolch.fn.isDate = function (val) {
    var pattern = /\\d\\d\\.\\d\\d\\.\\d\\d/;
    var isDate = pattern.test(val);
    return isDate;
};

strolch.fn.isTime = function (val) {
    var pattern = /[0-2][0-9]:[0-5][0-9]/;
    var isTime = pattern.test(val);
    return isTime;
};

strolch.fn.isEmail = function (val) {
    var pattern = /([a-zA-Z0-9_\-])([a-zA-Z0-9_\-\.]*)\+?([a-zA-Z0-9_\-])([a-zA-Z0-9_\-\.]*)?@(\[((25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\.){3}|((([a-zA-Z0-9\-]+)\.)+))([a-zA-Z]{2,}|(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])])/;
    var isEmail = pattern.test(val);
    return isEmail;
};

strolch.fn.equalsArray = function (a, b) {
    return $(a).not(b).length === 0 && $(b).not(a).length === 0;
};


/*
 * hack for multiple modals over each other - until bootstrap fixes this
 */
strolch.fn.multipleModalsHack = function () {

    var modal = $('.modal');

    modal.on('hidden.bs.modal', function (event) {
        $(this).removeClass('fv-modal-stack');
        var body = $('body');
        body.data('fv_open_modals', body.data('fv_open_modals') - 1);
    });

    modal.on('shown.bs.modal', function (event) {

        var body = $('body');

        // keep track of the number of open modals
        if (typeof( body.data('fv_open_modals') ) == 'undefined') {
            body.data('fv_open_modals', 0);
        }

        // if the z-index of this modal has been set, ignore.
        if ($(this).hasClass('fv-modal-stack')) {
            return;
        }

        $(this).addClass('fv-modal-stack');
        body.data('fv_open_modals', body.data('fv_open_modals') + 1);
        $(this).css('z-index', 1040 + (10 * body.data('fv_open_modals')));

        var modalBackDrop = $('.modal-backdrop');
        modalBackDrop.not('.fv-modal-stack').css('z-index', 1039 + (10 * body.data('fv_open_modals')));
        modalBackDrop.not('fv-modal-stack').addClass('fv-modal-stack');
    });
};

package cz.bbn.cerberus.dssetting.ui.components.tab;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.receivers.MemoryBuffer;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class DsSettingDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<DsSettingDto> appBinderOperation;
    private final List<UserDto> userList;
    private final boolean isDialog;
    private final AppEnv appEnv;
    private final boolean readOnly;

    public DsSettingDetailTabComponent(AppBinderOperation<DsSettingDto> appBinderOperation, List<UserDto> userList,
                                       boolean isDialog, AppEnv appEnv, boolean readOnly) {
        this.appBinderOperation = appBinderOperation;
        this.userList = userList;
        this.isDialog = isDialog;
        this.appEnv = appEnv;
        this.readOnly = readOnly;
        init();
    }

    private void init() {
        setSizeFull();

        appBinderOperation.getBinder().setBean(appBinderOperation.getDto());

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("Recipient ds id"));
        id.setMaxLength(20);
        id.setReadOnly(appBinderOperation.getDto().getId() != null);
        appBinderOperation.getBinder().forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new MinMaxValidator(2, 20))
                .bind(DsSettingDto::getId, DsSettingDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        appBinderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(DsSettingDto::getName, DsSettingDto::setName);
        formLayout.add(name);
        appBinderOperation.getBinder().forField(name).bind(DsSettingDto::getName, DsSettingDto::setName);

        ComboBox<UserDto> user = new ComboBox<>(Transl.get("Owner"));
        user.setItems(userList);
        user.setItemLabelGenerator(UserDto::getName);
        appBinderOperation.getBinder().forField(user).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(DsSettingDto::getUserDto, DsSettingDto::setUserDto);

        if (appBinderOperation.getBinder().getBean().getId() == null) {
            user.setValue(SecurityUtils.appUserToUserDto());
        }
        formLayout.add(user);

        TextField login = new TextField(Transl.get("Login"));
        login.setMaxLength(100);
        login.setReadOnly(appBinderOperation.getDto().isLoginByCertificate());

        PasswordField password = new PasswordField(Transl.get("Password"));
        password.setMaxLength(100);
        password.setRevealButtonVisible(false);
        password.setReadOnly(appBinderOperation.getDto().isLoginByCertificate());
        password.getElement().setAttribute("autocomplete", "new-password");

        if (appBinderOperation.getDto().isLoginByCertificate()) {
            appBinderOperation.getBinder().forField(login).bind(DsSettingDto::getLogin, DsSettingDto::setLogin);
            appBinderOperation.getBinder().forField(password)
                    .bind(DsSettingDto::getPassword, DsSettingDto::setPassword);
        } else {
            appBinderOperation.getBinder().forField(login).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(DsSettingDto::getLogin, DsSettingDto::setLogin);
            appBinderOperation.getBinder().forField(password)
                    .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(DsSettingDto::getPassword, DsSettingDto::setPassword);
        }

        MemoryBuffer memoryBuffer = new MemoryBuffer();
        Upload certificate = new Upload(memoryBuffer);
        certificate.setDropAllowed(true);
        int maxFileSizeInBytes = 20 * 1024 * 1024; // 20MB
        certificate.setMaxFileSize(maxFileSizeInBytes);
        certificate.setAcceptedFileTypes(".pem", ".cert");

        certificate.addFileRejectedListener(event -> {
            String errorMessage = event.getErrorMessage();
            ErrorNotification.show(Transl.get(errorMessage), appEnv);
        });

        Checkbox loginByCertificate = new Checkbox(Transl.get("Login by certificate"));
        appBinderOperation.getBinder().forField(loginByCertificate)
                .bind(DsSettingDto::isLoginByCertificate, DsSettingDto::setLoginByCertificate);
        Button uploadButton = VaadinComponents.getButton(Transl.get("Upload"));
        certificate.setUploadButton(uploadButton);

        loginByCertificate.addValueChangeListener(event -> {
            if (event.getValue().booleanValue()) {
                login.setEnabled(false);
                password.setEnabled(false);
                appBinderOperation.getBinder().forField(login).bind(DsSettingDto::getLogin, DsSettingDto::setLogin);
                appBinderOperation.getBinder().forField(password)
                        .bind(DsSettingDto::getPassword, DsSettingDto::setPassword);

            } else {
                login.setEnabled(true);
                password.setEnabled(true);
                appBinderOperation.getBinder().forField(login).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(DsSettingDto::getLogin, DsSettingDto::setLogin);
                appBinderOperation.getBinder().forField(password)
                        .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(DsSettingDto::getPassword, DsSettingDto::setPassword);
            }
            appBinderOperation.getBinder().validate();
        });

        certificate.addSucceededListener(event -> {
            if (!loginByCertificate.getValue().booleanValue()) {
                log.error("Upload is not allowed. Please change checkbox login by certificate");
            } else {
                appBinderOperation.getDto().setCertificate(memoryBuffer.getInputStream());
            }
        });

        formLayout.add(login, password, loginByCertificate, certificate);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        add(formLayout);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        appBinderOperation.getBinder().forField(description)
                .bind(DsSettingDto::getDescription, DsSettingDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        add(description);

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (readOnly) {
            name.setReadOnly(true);
            user.setReadOnly(true);
            login.setReadOnly(true);
            password.setReadOnly(true);
            loginByCertificate.setReadOnly(true);
            uploadButton.setEnabled(false);
            description.setReadOnly(true);
            uploadButton.setEnabled(false);
        }
    }
}

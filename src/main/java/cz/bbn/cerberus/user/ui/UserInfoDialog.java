package cz.bbn.cerberus.user.ui;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserComponentOperation;
import cz.bbn.cerberus.user.UserValues;
import cz.bbn.cerberus.user.ui.component.UserDialogInfoComponent;
import cz.bbn.cerberus.usermessage.UserMessageService;

public class UserInfoDialog extends AppDialog {

    private final UserMessageService userMessageService;
    private final MainLayout mainLayout;
    private final UserComponentOperation userComponentOperation;
    private final UserValues userValues;

    public UserInfoDialog(UserMessageService userMessageService, MainLayout mainLayout,
                          UserComponentOperation userComponentOperation, UserValues userValues) {
        this.userMessageService = userMessageService;
        this.mainLayout = mainLayout;
        this.userComponentOperation = userComponentOperation;
        this.userValues = userValues;
        init();
    }

    void init() {
        setTitle(Transl.get("User info"));
        setWidth("100em");

        UserDialogInfoComponent userDialogInfoComponent = new UserDialogInfoComponent(userValues);
        userDialogInfoComponent.initComponent();
        userDialogInfoComponent.setMinWidth("70%");
        setContent(userDialogInfoComponent);

        Button cancelButton = VaadinComponents.getCloseButton();
        cancelButton.addClickListener(buttonClickEvent ->
                this.close());

        Button randomMessage = VaadinComponents.getButton(Transl.get("Send random message"));
        randomMessage.setDisableOnClick(true);
        randomMessage.addClickListener(buttonClickEvent -> {
            userMessageService.randMessageToUser(SecurityUtils.getCurrentUserId());
            mainLayout.refreshMessageCount();
            randomMessage.setEnabled(true);
        });
        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent ->{
            userComponentOperation.saveUserActiveRoleLSet(userDialogInfoComponent.getRole().getSelectedItems());
            userValues.setTurnOffTranslations(userDialogInfoComponent.getTurnOffTranslations().getValue());
        });
        addButtons(cancelButton, randomMessage, submit);
    }
}

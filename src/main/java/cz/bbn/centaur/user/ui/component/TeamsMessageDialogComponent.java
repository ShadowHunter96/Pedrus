package cz.bbn.cerberus.user.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.azure.AzureGraphService;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

public class TeamsMessageDialogComponent extends AppDialog {

    private final AzureGraphService azureGraphService;
    private final UserDto userDto;

    public TeamsMessageDialogComponent(AzureGraphService azureGraphService, UserDto userDto) {
        this.azureGraphService = azureGraphService;
        this.userDto = userDto;
        initComponent();
    }

    private void initComponent() {
        setTitle(Transl.get("Send teams message"));

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);

        TextField userName = new TextField(Transl.get("Receiver"));
        userName.setReadOnly(true);
        userName.setValue(userDto.getName());
        TextArea message = new TextArea(Transl.get("Message to send"));
        message.setWidth("calc(60em - 10px)");
        message.setHeight("15em");
        message.setMaxWidth("calc(100% - 10px)");
        message.setMaxHeight("100%");
        verticalLayout.add(userName, message);
        setContent(verticalLayout);

        Button send = VaadinComponents.getButton(Transl.get("Send"));
        send.addClickListener(buttonClickEvent -> {
            azureGraphService.createChat(userDto.getLogin(), message.getValue());
            this.close();
        });
        addButtons(send);
        addCloseButton();
    }
}

package cz.bbn.cerberus.management.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.management.ManagementComponentOperation;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;

public class OwnerDialogComponent extends AppDialog {

    private final List<UserDto> userDtoList;
    private final ManagementComponentOperation managementComponentOperation;
    private final AppEnv appEnv;

    private ComboBox<UserDto> owner;
    private MultiSelectComboBox<ObjectType> objectType;
    private ComboBox<UserDto> newOwner;

    public OwnerDialogComponent(List<UserDto> userDtoList, ManagementComponentOperation managementComponentOperation,
                                AppEnv appEnv) {
        this.userDtoList = userDtoList;
        this.managementComponentOperation = managementComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent(){
        setTitle(Transl.get("Bulk owner transfer"));
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        VerticalLayout leftLayout = new VerticalLayout();
        owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItems(userDtoList);
        owner.setItemLabelGenerator(UserDto::getName);
        leftLayout.add(owner);

        objectType = new MultiSelectComboBox<>(Transl.get("Domain"));
        objectType.setItems(ObjectType.getEntityType());
        objectType.setItemLabelGenerator(objectType -> Transl.get(objectType.name()));
        leftLayout.add(objectType);

        VerticalLayout rightLayout = new VerticalLayout();
        newOwner = new ComboBox<>(Transl.get("New owner"));
        newOwner.setItems(userDtoList);
        newOwner.setItemLabelGenerator(userDto -> userDto.getName());
        rightLayout.add(newOwner);

        horizontalLayout.add(leftLayout, rightLayout);

        setContent(horizontalLayout);

        addCloseButton();
        Button submit = new Button(Transl.get("Transfer"));
        submit.addClickListener(buttonClickEvent -> {
            managementComponentOperation.bulkSave(objectType.getValue(), owner.getValue(), newOwner.getValue());
            SuccessNotification.showSavingSuccess(appEnv);
            this.close();
        });
        addButtons(submit);
    }
}

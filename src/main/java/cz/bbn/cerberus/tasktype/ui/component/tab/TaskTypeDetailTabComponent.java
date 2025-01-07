package cz.bbn.cerberus.tasktype.ui.component.tab;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class TaskTypeDetailTabComponent extends VerticalLayout {

    private final AppBinderOperation<TaskTypeDto> binderOperation;
    private final boolean isDialog;
    private final List<RoleDto> roleList;

    public TaskTypeDetailTabComponent(AppBinderOperation<TaskTypeDto> binderOperation,
                                      List<RoleDto> roleList, boolean isDialog) {
        this.binderOperation = binderOperation;
        this.isDialog = isDialog;
        this.roleList = roleList;
        initComponent();
    }

    private void initComponent() {
        this.setSizeFull();

        if (isDialog) {
            setMargin(false);
            setPadding(false);
        }

        if (binderOperation.getDto().getId() == null) {
            binderOperation.getDto().setArchived(false);
        }

        FormLayout formLayout = new FormLayout();

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        binderOperation.getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(TaskTypeDto::getName, TaskTypeDto::setName);
        formLayout.add(name);

        MultiSelectComboBox<RoleDto> roleComboBox = new MultiSelectComboBox<>(Transl.get("Role"));
        roleComboBox.setItems(roleList);
        roleComboBox.setItemLabelGenerator(RoleDto::getDescription);
        binderOperation.getBinder().forField(roleComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(TaskTypeDto::getRoleSet, TaskTypeDto::setRoleSet);
        formLayout.add(roleComboBox);

        MultiSelectComboBox<ObjectType> objectTypeComboBox = new MultiSelectComboBox<>(Transl.get("Entity type"));
        objectTypeComboBox.setItems(ObjectType.values());
        objectTypeComboBox.setItemLabelGenerator(this::getTranslObjectType);
        binderOperation.getBinder().forField(objectTypeComboBox)
                .bind(TaskTypeDto::getObjectTypeSet, TaskTypeDto::setObjectTypeSet);
        formLayout.add(objectTypeComboBox);

        Checkbox archived = new Checkbox(Transl.get("Archived"));
        binderOperation.getBinder().forField(archived).bind(TaskTypeDto::getArchived, TaskTypeDto::setArchived);
        formLayout.add(archived);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        binderOperation.getBinder().forField(description)
                .bind(TaskTypeDto::getDescription, TaskTypeDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        add(formLayout, description);

        if (!SecurityUtils.hasPermission(Permission.TASK_TYPE_EDIT)) {
            name.setReadOnly(true);
            description.setReadOnly(true);
            roleComboBox.setReadOnly(true);
            objectTypeComboBox.setReadOnly(true);
            archived.setReadOnly(true);
        }

        binderOperation.getBinder().setBean(binderOperation.getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
    }

    private String getTranslObjectType(ObjectType objectType) {
        return Transl.get(objectType.name());
    }
}

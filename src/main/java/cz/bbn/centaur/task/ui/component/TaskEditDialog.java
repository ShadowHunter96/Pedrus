package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.checkbox.CheckboxGroup;
import com.vaadin.flow.component.checkbox.CheckboxGroupVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideBarCountUpdateAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.IntegerValueMinMaxValidator;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.task.TaskOperationInterface;
import cz.bbn.cerberus.task.dto.NotifyFrequency;
import cz.bbn.cerberus.task.dto.SendTask;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskColor;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.dto.TaskState;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFrequency;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.SerializationUtils;

import java.time.DayOfWeek;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

public class TaskEditDialog extends AppDialog {

    private final TaskDto dto;
    private final TaskDto originalDto;
    private final AppInfiniteGrid<?> grid;
    private final TaskOperationInterface taskComponentOperation;
    private final SlideBarCountUpdateAction getCountUpdateAction;
    private final TaskEntityType taskEntityType;
    private final boolean isTask;
    private final boolean isSchedule;
    private final boolean isTemplate;

    private TaskChecklistComponent taskChecklistComponent;
    private List<TaskCheckDto> taskCheckDtoList;
    private boolean canEdit;
    private boolean canPartiallyEdit;

    private final Binder<TaskDto> binder = new Binder<>();


    public TaskEditDialog(TaskDto dto, AppInfiniteGrid<?> grid, TaskOperationInterface taskComponentOperation,
                          SlideBarCountUpdateAction getCountUpdateAction, TaskEntityType taskEntityType) {
        this.dto = dto;
        this.originalDto = SerializationUtils.clone(dto);
        this.grid = grid;
        this.taskComponentOperation = taskComponentOperation;
        this.getCountUpdateAction = getCountUpdateAction;
        this.taskEntityType = taskEntityType;
        this.isTask = taskEntityType == TaskEntityType.TASK;
        this.isSchedule = taskEntityType == TaskEntityType.SCHEDULE;
        this.isTemplate = taskEntityType == TaskEntityType.TEMPLATE;
        getCanEditPermission();
        initComponent();
    }

    public void initComponent() {

        setTitle(dto.getId() == null ? Transl.get(taskEntityType.getNewText()) :
                Transl.get(taskEntityType.getEditText()));

        if (dto.getNotifyFrequency() == null) {
            dto.setNotifyFrequency(NotifyFrequency.NEVER);
        }

        if (dto.getColor() == null) {
            dto.setColor(TaskColor.NONE);
        }

        binder.setBean(dto);

        VerticalLayout verticalLayout = new VerticalLayout();
        FormLayout formLayout = new FormLayout();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        DateTimePicker dateTimePicker = VaadinComponents.getDateTimePicker(Transl.get("Target date"), dto.getDate());

        if (isTask) {
            binder.forField(dateTimePicker).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getDate, TaskDto::setDate);
            formLayout.add(dateTimePicker);
        }

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        if (isTask || isSchedule) {
            binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getName, TaskDto::setName);
        } else {
            binder.forField(name).bind(TaskDto::getName, TaskDto::setName);
        }
        formLayout.add(name);

        TextField createdByUser = new TextField(Transl.get("Created by"));
        createdByUser.setReadOnly(true);
        if (dto.getUserDto() != null) {
            createdByUser.setValue(dto.getUserDto().getName());
        }
        formLayout.add(createdByUser);

        MultiSelectComboBox<UserDto> userComboBox = new MultiSelectComboBox<>(Transl.get("Assigned users"));
        userComboBox.setItems(taskComponentOperation.getAllowedUsers());
        userComboBox.setItemLabelGenerator(UserDto::getName);
        binder.forField(userComboBox).bind(TaskDto::getUserDtoSet, TaskDto::setUserDtoSet);
        formLayout.add(userComboBox);

        MultiSelectComboBox<RoleDto> roleComboBox = new MultiSelectComboBox<>(Transl.get("Assigned roles"));
        roleComboBox.setItems(taskComponentOperation.getAllowedRoles());
        roleComboBox.setItemLabelGenerator(RoleDto::getDescription);
        binder.forField(roleComboBox).bind(TaskDto::getRoleDtoSet, TaskDto::setRoleDtoSet);
        formLayout.add(roleComboBox);

        ComboBox<TaskState> stateComboBox = new ComboBox<>(Transl.get("State"));
        stateComboBox.setItems(TaskState.values());
        stateComboBox.setItemLabelGenerator(TaskState::getTranslatedValue);

        if ((dto.getId() == null && dto.getState() == null) || isTemplate || isSchedule) {
            dto.setState(TaskState.NEW);
            stateComboBox.setReadOnly(true);
        }

        if (isTask || isSchedule) {
            binder.forField(stateComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getState, TaskDto::setState);
        } else {
            binder.forField(stateComboBox).bind(TaskDto::getState, TaskDto::setState);
        }
        formLayout.add(stateComboBox);

        ComboBox<ObjectType> objectType = new ComboBox<>(Transl.get("Entity type"));
        ComboBox<ItemDto> entity = new ComboBox<>(Transl.get("Entity"));

        objectType.setItems(ObjectType.values());
        objectType.setItemLabelGenerator(objectType1 -> Transl.get(objectType1.name()));
        binder.forField(objectType).bind(TaskDto::getObjectType, TaskDto::setObjectType);
        objectType.addValueChangeListener(task -> {
            entity.setItems(taskComponentOperation.loadItemDtoListByObjectType(task.getValue()));
            if (task.getValue() == null || ObjectType.ANY == task.getValue()) {
                entity.setRequired(false);
                entity.setReadOnly(true);
            } else {
                if (taskEntityType == TaskEntityType.TASK) {
                    entity.setRequired(true);
                }
                entity.setReadOnly(false);
            }
        });
        formLayout.add(objectType);

        List<ItemDto> itemDtoList = dto.getObjectType() != null ?
                taskComponentOperation.loadItemDtoListByObjectType(dto.getObjectType()) : new ArrayList<>();
        entity.setItems(itemDtoList);
        entity.setItemLabelGenerator(ItemDto::getName);
        dto.setItemDto(itemDtoList.stream().filter(itemDto ->
                itemDto.getId().equals(dto.getObjectId())).findAny().orElse(null));
        if (isTask || isSchedule) {
            binder.forField(entity).asRequired((itemDto, valueContext) -> {
                if (objectType.getValue() != null && itemDto == null) {
                    return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                } else {
                    return ValidationResult.ok();
                }
            }).bind(TaskDto::getItemDto, TaskDto::setItemDto);
            entity.setRequired(objectType.getValue() != null);
            entity.setReadOnly(objectType.getValue() == null);
        } else {
            binder.forField(entity).bind(TaskDto::getItemDto, TaskDto::setItemDto);
        }
        formLayout.add(entity);

        IntegerField daysToNotify = new IntegerField(Transl.get("Days to notify"));
        if (isTask || isSchedule) {
            binder.forField(daysToNotify).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getDaysToNotify, TaskDto::setDaysToNotify);
        } else {
            binder.forField(daysToNotify).bind(TaskDto::getDaysToNotify, TaskDto::setDaysToNotify);
        }
        formLayout.add(daysToNotify);

        ComboBox<NotifyFrequency> notifyFrequency = new ComboBox<>(Transl.get("Notify frequency"));
        notifyFrequency.setItems(NotifyFrequency.values());
        notifyFrequency.setItemLabelGenerator(NotifyFrequency::getTranslatedValue);
        binder.forField(notifyFrequency).bind(TaskDto::getNotifyFrequency, TaskDto::setNotifyFrequency);
        formLayout.add(notifyFrequency);

        ComboBox<TaskColor> taskColor = new ComboBox<>(Transl.get("Priority"));
        taskColor.setItems(TaskColor.values());
        taskColor.setItemLabelGenerator(TaskColor::getTranslatedValue);
        binder.forField(taskColor).bind(TaskDto::getColor, TaskDto::setColor);
        formLayout.add(taskColor);

        ComboBox<UserDto> assignee = new ComboBox<>(Transl.get("Solver"));
        assignee.setReadOnly(isTemplate || isSchedule ||
                (!dto.getState().equals(TaskState.NEW) && !dto.getState().equals(TaskState.ASSIGNED)));
        assignee.setItems(taskComponentOperation.getAssigneeUserList(dto));
        assignee.setItemLabelGenerator(item -> item.getName());

        if (!isTemplate && !isSchedule) {
            binder.forField(assignee).bind(TaskDto::getAssignee, TaskDto::setAssignee);
        }
        formLayout.add(assignee);

        stateComboBox.addValueChangeListener(e -> {
            if (e.getValue() != null && e.getValue() == TaskState.ASSIGNED) {
                dto.setAssignee(SecurityUtils.getCurrentUserDto());
                assignee.setValue(SecurityUtils.getCurrentUserDto());
            }
        });

        ComboBox<RoleDto> roleDtoComboBox = new ComboBox<>(Transl.get("Created for role"));
        if (taskEntityType == TaskEntityType.TEMPLATE) {
            roleDtoComboBox.setItems(taskComponentOperation.getAllowedRoles());
            roleDtoComboBox.setItemLabelGenerator(RoleDto::getDescription);
            binder.forField(roleDtoComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getAllowedRole, TaskDto::setAllowedRole);
            formLayout.add(roleDtoComboBox);
        }

        ComboBox<TaskScheduleFrequency> frequency = new ComboBox<>(Transl.get("Frequency"));
        ComboBox<DayOfWeek> dayOfWeekFrequency = new ComboBox<>(Transl.get("Frequency"));

        if (isSchedule) {
            frequency.setItems(TaskScheduleFrequency.values());
            frequency.setItemLabelGenerator(TaskScheduleFrequency::getTranslatedValue);
            binder.forField(frequency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getFrequency, TaskDto::setFrequency);
            formLayout.add(frequency);

            IntegerField creationDay = new IntegerField(Transl.get("Day in month"));
            if (dto.getFrequency() != TaskScheduleFrequency.WEEKLY) {
                binder.forField(creationDay).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .withValidator(new IntegerValueMinMaxValidator(1, 31))
                        .bind(TaskDto::getCreationDay, TaskDto::setCreationDay);
            }
            formLayout.add(creationDay);

            dayOfWeekFrequency.setItems(DayOfWeek.values());
            dayOfWeekFrequency.setItemLabelGenerator(this::getDayTransValue);
            binder.forField(dayOfWeekFrequency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(this::getCreationDay, this::setCreationDay);
            formLayout.add(dayOfWeekFrequency);

            if (dto.getFrequency() != TaskScheduleFrequency.WEEKLY) {
                binder.forField(creationDay).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .withValidator(new IntegerValueMinMaxValidator(1, 31))
                        .bind(TaskDto::getCreationDay, TaskDto::setCreationDay);
                dayOfWeekFrequency.setVisible(false);
            } else {
                binder.forField(dayOfWeekFrequency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(this::getCreationDay, this::setCreationDay);
                creationDay.setVisible(false);
            }

            frequency.addValueChangeListener(e -> {
                if (e.getValue() == TaskScheduleFrequency.WEEKLY) {
                    dayOfWeekFrequency.setVisible(true);
                    creationDay.setVisible(false);
                    binder.removeBinding(creationDay);
                    binder.forField(dayOfWeekFrequency).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                            .bind(this::getCreationDay, this::setCreationDay);
                } else {
                    dayOfWeekFrequency.setVisible(false);
                    creationDay.setVisible(true);
                    binder.removeBinding(dayOfWeekFrequency);
                    binder.forField(creationDay).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                            .withValidator(new IntegerValueMinMaxValidator(1, 31))
                            .bind(TaskDto::getCreationDay, TaskDto::setCreationDay);
                }
            });
        }

        ComboBox<TaskTypeDto> taskType = new ComboBox<>(Transl.get("Task type"));
        taskType.setItems(taskComponentOperation.getAllowedTaskTypeList(dto.getObjectType()));
        taskType.setItemLabelGenerator(TaskTypeDto::getName);
        if (isTask || isSchedule) {
            binder.forField(taskType).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(TaskDto::getTaskType, TaskDto::setTaskType);
        } else {
            binder.forField(taskType).bind(TaskDto::getTaskType, TaskDto::setTaskType);
        }
        formLayout.add(taskType);

        objectType.addValueChangeListener(e -> {
            taskType.setItems(taskComponentOperation.getAllowedTaskTypeList(e.getValue()));
            taskType.setValue(null);
        });

        verticalLayout.add(formLayout);

        CheckboxGroup<SendTask> sendTask = new CheckboxGroup<>(Transl.get("Send a notification by"));
        sendTask.setItems(SendTask.values());
        sendTask.setItemLabelGenerator(task -> Transl.get(task.getName()));
        sendTask.addThemeVariants(CheckboxGroupVariant.LUMO_HELPER_ABOVE_FIELD);
        if (dto.getSendTaskSet() != null) {
            sendTask.select(dto.getSendTaskSet());
        }
        binder.forField(sendTask).asRequired(Transl.get("You have to choose one"))
                .bind(TaskDto::getSendTaskSet, TaskDto::setSendTaskSet);
        verticalLayout.add(sendTask);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        binder.forField(description)
                .bind(TaskDto::getDescription, TaskDto::setDescription);
        description.setWidthFull();
        description.setHeight("6em");
        verticalLayout.add(description);

        Checkbox sendToOutlook = new Checkbox(Transl.get("Send to Outlook"));
        binder.forField(sendToOutlook).bind(TaskDto::isSendToOutlook, TaskDto::setSendToOutlook);
        sendToOutlook.setEnabled(dto.getId() == null);
        verticalLayout.add(sendToOutlook);

        if (dto.getTaskCheckDtoList() == null || dto.getTaskCheckDtoList().isEmpty()) {
            taskCheckDtoList = taskComponentOperation.getTaskCheckList(dto.getId());
        } else {
            taskCheckDtoList = dto.getTaskCheckDtoList();
        }

        originalDto.setTaskCheckDtoList(new ArrayList<>());
        taskCheckDtoList.forEach(taskCheckDto ->
                originalDto.getTaskCheckDtoList().add(SerializationUtils.clone(taskCheckDto)));

        taskChecklistComponent = new TaskChecklistComponent(taskCheckDtoList, dto);
        verticalLayout.add(taskChecklistComponent);

        if (dto.getId() == null) {
            Button addCheckList = VaadinComponents.getButton(Transl.get("Add checkbox list"), VaadinIcon.PLUS.create());
            verticalLayout.add(addCheckList);
            addCheckList.setDisableOnClick(true);
            addCheckList.addClickListener(e -> {
                new TaskChecklistDialog(taskCheckDtoList, getSetCheckListAction(dto)).open();
                addCheckList.setEnabled(true);
            });
            verticalLayout.add(addCheckList);
        }

        dateTimePicker.addValueChangeListener(e -> taskChecklistComponent.generateComponent(taskCheckDtoList, dto));

        setContent(verticalLayout);

        Button saveButton = VaadinComponents.getSubmitButton();
        saveButton.setDisableOnClick(true);
        saveButton.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                dto.setTaskCheckDtoList(taskChecklistComponent.getTaskCheckDtoList());
                taskComponentOperation.getSaveAction().saveItem(dto, originalDto);
                showWarning(false);
                this.close();
                if (grid != null) {
                    grid.loadData();
                    if (getCountUpdateAction != null) {
                        getCountUpdateAction.updateCount();
                    }
                }
            }
            saveButton.setEnabled(true);
        });

        addCloseButton();
        if (!canEdit) {
            dateTimePicker.setReadOnly(true);
            name.setReadOnly(true);
            objectType.setReadOnly(true);
            entity.setReadOnly(true);
            sendTask.setReadOnly(true);
            description.setReadOnly(true);
            sendToOutlook.setReadOnly(true);
            daysToNotify.setReadOnly(true);
            notifyFrequency.setReadOnly(true);
            taskColor.setReadOnly(true);
            roleDtoComboBox.setReadOnly(true);
            frequency.setReadOnly(true);
            dayOfWeekFrequency.setReadOnly(true);
            userComboBox.setReadOnly(true);
            roleComboBox.setReadOnly(true);
            stateComboBox.setReadOnly(true);
            taskType.setReadOnly(true);
        }

        if (canPartiallyEdit) {
            stateComboBox.setReadOnly(false);
            assignee.setReadOnly(false);
        }

        if (canEdit || canPartiallyEdit) {
            addSubmitButton(saveButton);
        }

        showWarning(true);

    }

    private String getDayTransValue(DayOfWeek dayOfWeek) {
        return dayOfWeek.getDisplayName(TextStyle.FULL, Locale.forLanguageTag("cs"));
    }

    private void setCreationDay(TaskDto taskDto, DayOfWeek dayOfWeek) {
        if (dayOfWeek != null) {
            taskDto.setCreationDay(dayOfWeek.getValue());
        }
    }

    private DayOfWeek getCreationDay(TaskDto taskDto) {
        if (taskDto.getCreationDay() != null && taskDto.getCreationDay() < 8) {
            return DayOfWeek.values()[taskDto.getCreationDay() - 1];
        }
        return null;
    }

    private SetChecklistAction getSetCheckListAction(TaskDto taskDto) {
        return e -> {
            this.taskCheckDtoList = e;
            taskChecklistComponent.generateComponent(e, taskDto);
        };
    }

    private void getCanEditPermission() {
        if (taskEntityType == TaskEntityType.TASK) {
            Set<String> currentUserRoleSet = SecurityUtils.getCurrentUser().getActiveRoleSet();
            Set<String> dtoRoleSet = new HashSet<>();
            Set<Long> assignedUserIdSet = new HashSet<>();
            if (dto.getRoleDtoSet() == null) {
                dto.setRoleDtoSet(new HashSet<>());
            }
            if (dto.getUserDtoSet() == null) {
                dto.setUserDtoSet(new HashSet<>());
            }
            for (RoleDto roleDto : dto.getRoleDtoSet()) {
                dtoRoleSet.add(roleDto.getId());
            }
            for (UserDto userDto : dto.getUserDtoSet()) {
                assignedUserIdSet.add(userDto.getId());
            }
            this.canEdit = SecurityUtils.hasPermission(Permission.TASK_EDIT)
                    && ((dto.getUserDto() == null || (
                    dto.getUserDto() != null &&
                            dto.getUserDto().getId().equals(SecurityUtils.getCurrentUserId())))
                    || (
                    dto.getAssignee() != null
                            && dto.getAssignee().getId().equals(SecurityUtils.getCurrentUserId())));
            this.canPartiallyEdit = SecurityUtils.hasPermission(Permission.TASK_EDIT)
                    && (!Collections.disjoint(currentUserRoleSet, dtoRoleSet)
                    || assignedUserIdSet.contains(SecurityUtils.getCurrentUserId()));
        } else {
            canEdit = (dto.getUserDto() == null || dto.getUserDto().getId().equals(SecurityUtils.getCurrentUserId()))
                    && SecurityUtils.hasPermission(Permission.TASK_TEMPLATE_EDIT);
        }
    }

}

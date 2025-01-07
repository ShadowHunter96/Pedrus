package cz.bbn.cerberus.task;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import cz.bbn.cerberus.azure.AzureGraphService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.task.dto.SendTask;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.dto.TaskFilterDto;
import cz.bbn.cerberus.task.ui.component.TaskEditDialog;
import cz.bbn.cerberus.task.ui.component.TaskFilterComponent;
import cz.bbn.cerberus.task.ui.component.TaskGridComponent;
import cz.bbn.cerberus.task.ui.component.TaskPickTemplateDialog;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.tasktemplate.TaskTemplateComponentOperation;
import cz.bbn.cerberus.tasktype.TaskTypeService;
import cz.bbn.cerberus.tasktype.dto.TaskTypeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class TaskComponentOperation implements TaskOperationInterface {

    private final TaskService taskService;
    private final AzureGraphService azureGraphService;
    private final AppEnv appEnv;
    private final UserService userService;
    private final ListService listService;
    private final RoleService roleService;
    private final TaskTemplateComponentOperation templateComponentOperation;
    private final TaskTypeService taskTypeService;

    public TaskComponentOperation(TaskService taskService, AzureGraphService azureGraphService,
                                  AppEnv appEnv, UserService userService, ListService listService,
                                  RoleService roleService, TaskTemplateComponentOperation templateComponentOperation,
                                  TaskTypeService taskTypeService) {
        this.taskService = taskService;
        this.azureGraphService = azureGraphService;
        this.appEnv = appEnv;
        this.userService = userService;
        this.listService = listService;
        this.roleService = roleService;
        this.templateComponentOperation = templateComponentOperation;
        this.taskTypeService = taskTypeService;
    }

    public SaveAction<TaskDto> getSaveAction() {
        return (dto, originalDto) -> {
            if (dto.getObjectType() == ObjectType.ANY) {
                dto.setObjectType(null);
            }
            if (dto.getObjectType() != null) {
                dto.setObjectId(dto.getItemDto().getId());
            }
            try {
                if (dto.getId() == null) {
                    taskService.saveTask(dto);
                    if (dto.isSendToOutlook()) {
                        azureGraphService.createOutlookEvent(dto);
                    }
                } else {
                    taskService.updateTask(dto, originalDto);
                }
                SuccessNotification.show("Save successfull", appEnv);
            } catch (SystemException e) {
                log.error("Save task error", e);
                ErrorNotification.show(e, appEnv);
            }
        };
    }

    @Override
    public List<ItemDto> loadItemDtoListByObjectType(ObjectType objectType) {
        List<ItemDto> list = new ArrayList<>();
        if (objectType == null) {
            return list;
        }
        switch (objectType) {
            case SUBJECT -> list.addAll(
                    listService.getSubjectDtoList().stream()
                            .map(item -> new ItemDto(item.getId(), item.getName()))
                            .toList());
            case OPPORTUNITY -> list.addAll(
                    listService.getOpportunityDtoList()
                            .stream()
                            .map(item -> new ItemDto(item.getId(), item.getName()))
                            .toList());
            case CONTRACT -> list.addAll(
                    listService.getContractList()
                            .stream()
                            .map(item -> new ItemDto(item.getId(), item.getName()))
                            .toList());
            case PROJECT -> list.addAll(
                    listService.getProjectDtoList()
                            .stream()
                            .map(item -> new ItemDto(item.getId(), item.getName()))
                            .toList());
            case ASSET -> list.addAll(
                    listService.getAssetDtoList().stream()
                            .map(item -> new ItemDto(item.getId(), item.getName()))
                            .toList());
            case INVOICE -> list.addAll(
                    listService.getInvoiceDtoList().stream()
                            .map(item -> new ItemDto(String.valueOf(item.getId()),
                                    AppUtils.formatDate(item.getIssueDate())
                                            .concat(" ")
                                            .concat(item.getContractDto().getName()).concat(" ")
                                            .concat(item.getContractDto().getSubjectDto().getName())))
                            .toList());
            case OFFER -> list.addAll(
                    listService.getOfferDtoList().stream()
                            .map(item -> new ItemDto(item.getId(), item.getName()))
                            .toList());
            default -> {
            }

        }
        return list;
    }

    public ItemsAction<TaskDto> getItemsAction(TaskFilterComponent taskFilterComponent) {
        return (query, orderList) -> {
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("creationDate"));
            }
            TaskFilterDto filter = taskFilterComponent.getTaskFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return taskService.findTaskDtoPage(filter);
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                taskService.deleteTask(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public TaskDto getTaskDto(Long id) {
        try {
            return taskService.getTaskDto(id);
        } catch (SystemException e) {
            log.error("Get task error", e);
            ErrorNotification.show(e, appEnv);
            return new TaskDto();
        }
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getAddNewClickEvent(
            TaskGridComponent grid, boolean fromTemplate) {
        return getAddNewClickEvent(grid, null, null, null, null, fromTemplate);
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getAddNewClickEvent(
            TaskGridComponent grid, SubjectDto subjectDto, ObjectType objectType, String objectId,
            TaskSlideTabComponent taskSlideTabComponent, boolean fromTemplate) {
        return buttonClickEvent -> {
            TaskDto taskDto = new TaskDto();
            taskDto.setUserDto(SecurityUtils.getCurrentUserDto());
            taskDto.setUserDtoSet(new HashSet<>());
            Set<SendTask> sendTaskSet = new HashSet<>();
            sendTaskSet.add(SendTask.APP_NOTIFICATION);
            taskDto.setSendTaskSet(sendTaskSet);
            if (objectType != null) {
                taskDto.setObjectType(objectType);
                taskDto.setObjectId(objectId);
            }
            if (subjectDto != null) {
                taskDto.setName(Transl.get(ObjectType.SUBJECT.name()).concat(" - ").concat(subjectDto.getName()));
                taskDto.setSubjectId(subjectDto.getId());
            }
            if (fromTemplate) {
                TaskPickTemplateDialog dialog = new TaskPickTemplateDialog(
                        this, templateComponentOperation, grid,
                        taskSlideTabComponent != null ? taskSlideTabComponent.getCountUpdateAction() : null);
                dialog.open();
            } else {
                TaskEditDialog dialog = new TaskEditDialog(new TaskDto(), grid, this,
                        taskSlideTabComponent != null ? taskSlideTabComponent.getCountUpdateAction() : null,
                        TaskEntityType.TASK
                );
                dialog.open();
            }
        };
    }

    public int getTaskCount(TaskFilterDto filter) {
        return taskService.getTaskCount(filter);
    }

    @Override
    public List<UserDto> getAllowedUsers() {
        List<UserDto> allowedUsers = userService.findAllowedUserList();
        allowedUsers.sort(Comparator.comparing(UserDto::getName));
        return allowedUsers;
    }

    @Override
    public List<RoleDto> getAllowedRoles() {
        List<RoleDto> allowedRoles = roleService.findAll();
        allowedRoles.sort(Comparator.comparing(RoleDto::getId));
        return allowedRoles;
    }

    @Override
    public List<TaskCheckDto> getTaskCheckList(Long id) {
        if (id == null) {
            return new ArrayList<>();
        }
        List<TaskCheckDto> taskCheckDtoList = taskService.getTaskCheckListByTaskId(id);
        taskCheckDtoList.sort(Comparator.comparing(TaskCheckDto::getId));
        return taskCheckDtoList;
    }

    @Override
    public List<TaskTypeDto> getAllowedTaskTypeList(ObjectType objectType) {
        return taskTypeService.getAllowedTaskTypeList(objectType);
    }

    @Override
    public List<UserDto> getAssigneeUserList(TaskDto dto) {
        return taskService.getResolverUserList(dto);
    }
}

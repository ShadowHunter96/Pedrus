package cz.bbn.cerberus.tasktemplate;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import cz.bbn.cerberus.commons.AppUtils;
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
import cz.bbn.cerberus.task.TaskOperationInterface;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.ui.component.TaskEditDialog;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateDto;
import cz.bbn.cerberus.tasktemplate.dto.TaskTemplateFilterDto;
import cz.bbn.cerberus.tasktemplate.ui.component.TaskTemplateFilterComponent;
import cz.bbn.cerberus.tasktemplate.ui.component.TaskTemplateGridComponent;
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
import java.util.List;

@Component
@Slf4j
public class TaskTemplateComponentOperation implements TaskOperationInterface {

    private final TaskTemplateService taskTemplateService;
    private final UserService userService;
    private final RoleService roleService;
    private final ListService listService;
    private final TaskTypeService taskTypeService;
    private final AppEnv appEnv;

    public TaskTemplateComponentOperation(TaskTemplateService taskTemplateService, UserService userService,
                                          RoleService roleService, ListService listService,
                                          TaskTypeService taskTypeService, AppEnv appEnv) {
        this.taskTemplateService = taskTemplateService;
        this.userService = userService;
        this.roleService = roleService;
        this.listService = listService;
        this.taskTypeService = taskTypeService;
        this.appEnv = appEnv;
    }

    public TaskDto getTaskFromTemplate(TaskTemplateDto templateDto) {
        TaskDto taskDto = new TaskDto();
        taskDto.setId(templateDto.getId());
        taskDto.setName(templateDto.getName());
        taskDto.setDescription(templateDto.getDescription());
        taskDto.setObjectType(templateDto.getObjectType());
        taskDto.setObjectId(templateDto.getObjectId());
        taskDto.setItemDto(templateDto.getItemDto());
        taskDto.setSubjectId(templateDto.getSubjectId());
        taskDto.setUserDtoSet(templateDto.getUserDtoSet());
        taskDto.setRoleDtoSet(templateDto.getRoleDtoSet());
        taskDto.setState(templateDto.getState());
        taskDto.setDaysToNotify(templateDto.getDaysToNotify());
        taskDto.setNotifyFrequency(templateDto.getNotifyFrequency());
        taskDto.setColor(templateDto.getColor());
        taskDto.setAssignee(templateDto.getAssignee());
        taskDto.setAllowedRole(templateDto.getAllowedRole());
        taskDto.setTaskCheckDtoList(templateDto.getTaskCheckDtoList());
        taskDto.setSendTaskSet(templateDto.getSendTaskSet());
        taskDto.setSendToOutlook(Boolean.TRUE.equals(templateDto.isSendToOutlook()));
        taskDto.setTaskType(templateDto.getTaskType());
        taskDto.setUserDto(templateDto.getUserDto());
        return taskDto;
    }

    public TaskTemplateDto getTemplateFromTask(TaskDto taskDto) {
        TaskTemplateDto templateDto = new TaskTemplateDto();
        templateDto.setId(taskDto.getId());
        templateDto.setName(taskDto.getName());
        templateDto.setDescription(taskDto.getDescription());
        templateDto.setObjectType(taskDto.getObjectType());
        templateDto.setObjectId(taskDto.getObjectId());
        templateDto.setItemDto(taskDto.getItemDto());
        templateDto.setSubjectId(taskDto.getSubjectId());
        templateDto.setUserDtoSet(taskDto.getUserDtoSet());
        templateDto.setRoleDtoSet(taskDto.getRoleDtoSet());
        templateDto.setState(taskDto.getState());
        templateDto.setDaysToNotify(taskDto.getDaysToNotify());
        templateDto.setNotifyFrequency(taskDto.getNotifyFrequency());
        templateDto.setColor(taskDto.getColor());
        templateDto.setAssignee(taskDto.getAssignee());
        templateDto.setAllowedRole(taskDto.getAllowedRole());
        templateDto.setTaskCheckDtoList(taskDto.getTaskCheckDtoList());
        templateDto.setSendTaskSet(taskDto.getSendTaskSet());
        templateDto.setSendToOutlook(Boolean.TRUE.equals(taskDto.isSendToOutlook()));
        templateDto.setTaskType(taskDto.getTaskType());
        templateDto.setUserDto(taskDto.getUserDto());
        return templateDto;
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

    @Override
    public SaveAction<TaskDto> getSaveAction() {
        return (taskDto, taskOriginalDto) -> {
            TaskTemplateDto dto = getTemplateFromTask(taskDto);
            dto.setDeleted(Boolean.TRUE.equals(dto.isDeleted()));
            TaskTemplateDto originalDto = getTemplateFromTask(taskOriginalDto);
            if (dto.getObjectType() == ObjectType.ANY) {
                dto.setObjectType(null);
            }
            if (dto.getObjectType() != null) {
                dto.setObjectId(dto.getItemDto().getId());
            }
            try {
                if (dto.getId() == null) {
                    dto.setUserDto(SecurityUtils.getCurrentUserDto());
                    taskTemplateService.saveTemplate(dto);
                } else {
                    taskTemplateService.updateTemplate(dto, originalDto);
                }
                SuccessNotification.show("Save successfull", appEnv);
            } catch (SystemException e) {
                log.error("Save task template error", e);
                ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
            }
        };
    }

    @Override
    public List<TaskCheckDto> getTaskCheckList(Long id) {
        return taskTemplateService.getTaskChecklist(id);
    }

    @Override
    public List<TaskTypeDto> getAllowedTaskTypeList(ObjectType objectType) {
        return taskTypeService.getAllowedTaskTypeList(objectType);
    }

    @Override
    public List<UserDto> getAssigneeUserList(TaskDto dto) {
        return new ArrayList<>();
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                taskTemplateService.deleteById(id);
            } catch (SystemException e) {
                log.error(e.getMessage(), e);
                ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
            }
        };
    }

    public ItemsAction<TaskTemplateDto> getItemsAction(TaskTemplateFilterComponent filterComponent) {
        return (query, orderList) -> {
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("id"));
            }
            TaskTemplateFilterDto filter = filterComponent.getTaskTemplateFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return taskTemplateService.findTaskDtoPage(filter);
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getAddNewClickEvent(
            TaskTemplateGridComponent grid) {
        return e -> {
            TaskDto taskDto = new TaskDto();
            taskDto.setUserDto(SecurityUtils.getCurrentUserDto());
            new TaskEditDialog(taskDto, grid, this, null, TaskEntityType.TEMPLATE).open();
        };
    }

    public List<TaskTemplateDto> getAllowedTemplateList() {
        return taskTemplateService.getAllowedTemplateList();
    }
}

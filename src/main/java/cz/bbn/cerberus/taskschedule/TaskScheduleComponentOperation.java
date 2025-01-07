package cz.bbn.cerberus.taskschedule;

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
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleDto;
import cz.bbn.cerberus.taskschedule.dto.TaskScheduleFilterDto;
import cz.bbn.cerberus.taskschedule.ui.component.TaskScheduleFilterComponent;
import cz.bbn.cerberus.taskschedule.ui.component.TaskScheduleGridComponent;
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
public class TaskScheduleComponentOperation implements TaskOperationInterface {

    private final TaskScheduleService taskScheduleService;
    private final UserService userService;
    private final RoleService roleService;
    private final ListService listService;
    private final TaskTypeService taskTypeService;
    private final AppEnv appEnv;

    public TaskScheduleComponentOperation(TaskScheduleService taskScheduleService, UserService userService,
                                          RoleService roleService, ListService listService,
                                          TaskTypeService taskTypeService, AppEnv appEnv) {
        this.taskScheduleService = taskScheduleService;
        this.userService = userService;
        this.roleService = roleService;
        this.listService = listService;
        this.taskTypeService = taskTypeService;
        this.appEnv = appEnv;
    }

    public TaskDto getTaskFromSchedule(TaskScheduleDto scheduleDto) {
        TaskDto taskDto = new TaskDto();
        taskDto.setId(scheduleDto.getId());
        taskDto.setName(scheduleDto.getName());
        taskDto.setDescription(scheduleDto.getDescription());
        taskDto.setObjectType(scheduleDto.getObjectType());
        taskDto.setObjectId(scheduleDto.getObjectId());
        taskDto.setItemDto(scheduleDto.getItemDto());
        taskDto.setSubjectId(scheduleDto.getSubjectId());
        taskDto.setUserDtoSet(scheduleDto.getUserSet());
        taskDto.setRoleDtoSet(scheduleDto.getRoleSet());
        taskDto.setState(scheduleDto.getState());
        taskDto.setDaysToNotify(scheduleDto.getDaysToNotify());
        taskDto.setNotifyFrequency(scheduleDto.getNotifyFrequency());
        taskDto.setColor(scheduleDto.getColor());
        taskDto.setAssignee(scheduleDto.getAssignee());
        taskDto.setTaskCheckDtoList(scheduleDto.getTaskCheckList());
        taskDto.setSendTaskSet(scheduleDto.getSendTaskSet());
        taskDto.setSendToOutlook(Boolean.TRUE.equals(scheduleDto.isSendToOutlook()));
        taskDto.setUserDto(scheduleDto.getUserDto());
        taskDto.setFrequency(scheduleDto.getFrequency());
        taskDto.setTaskType(scheduleDto.getTaskType());
        taskDto.setCreationDay(scheduleDto.getCreationDay());
        return taskDto;
    }

    public TaskScheduleDto getScheduleFromTask(TaskDto taskDto) {
        TaskScheduleDto scheduleDto = new TaskScheduleDto();
        scheduleDto.setId(taskDto.getId());
        scheduleDto.setName(taskDto.getName());
        scheduleDto.setDescription(taskDto.getDescription());
        scheduleDto.setObjectType(taskDto.getObjectType());
        scheduleDto.setObjectId(taskDto.getObjectId());
        scheduleDto.setItemDto(taskDto.getItemDto());
        scheduleDto.setSubjectId(taskDto.getSubjectId());
        scheduleDto.setUserSet(taskDto.getUserDtoSet());
        scheduleDto.setRoleSet(taskDto.getRoleDtoSet());
        scheduleDto.setState(taskDto.getState());
        scheduleDto.setDaysToNotify(taskDto.getDaysToNotify());
        scheduleDto.setNotifyFrequency(taskDto.getNotifyFrequency());
        scheduleDto.setColor(taskDto.getColor());
        scheduleDto.setAssignee(taskDto.getAssignee());
        scheduleDto.setTaskCheckList(taskDto.getTaskCheckDtoList());
        scheduleDto.setSendTaskSet(taskDto.getSendTaskSet());
        scheduleDto.setSendToOutlook(Boolean.TRUE.equals(taskDto.isSendToOutlook()));
        scheduleDto.setUserDto(taskDto.getUserDto());
        scheduleDto.setFrequency(taskDto.getFrequency());
        scheduleDto.setTaskType(taskDto.getTaskType());
        scheduleDto.setCreationDay(taskDto.getCreationDay());
        return scheduleDto;
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
            TaskScheduleDto dto = getScheduleFromTask(taskDto);
            dto.setDeleted(Boolean.TRUE.equals(dto.isDeleted()));
            TaskScheduleDto originalDto = getScheduleFromTask(taskOriginalDto);
            if (dto.getObjectType() == ObjectType.ANY) {
                dto.setObjectType(null);
            }
            if (dto.getObjectType() != null) {
                dto.setObjectId(dto.getItemDto().getId());
            }
            try {
                if (dto.getId() == null) {
                    dto.setUserDto(SecurityUtils.getCurrentUserDto());
                    taskScheduleService.saveSchedule(dto);
                } else {
                    taskScheduleService.updateSchedule(dto, originalDto);
                }
                SuccessNotification.show("Save successfull", appEnv);
            } catch (SystemException e) {
                log.error("Save task schedule error", e);
                ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
            }
        };
    }

    @Override
    public List<TaskCheckDto> getTaskCheckList(Long id) {
        return taskScheduleService.getTaskChecklist(id);
    }

    @Override
    public List<TaskTypeDto> getAllowedTaskTypeList(ObjectType objectType) {
        return taskTypeService.getAllowedTaskTypeList(objectType);
    }

    @Override
    public List<UserDto> getAssigneeUserList(TaskDto dto) {
        return new ArrayList<>();
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getAddNewClickEvent(
            TaskScheduleGridComponent grid) {
        return e -> {
            TaskDto taskDto = new TaskDto();
            taskDto.setUserDto(SecurityUtils.getCurrentUserDto());
            new TaskEditDialog(taskDto, grid, this, null, TaskEntityType.SCHEDULE).open();
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                taskScheduleService.deleteById(id);
            } catch (SystemException e) {
                log.error(e.getMessage(), e);
                ErrorNotification.show(Transl.get(e.getMessage()), appEnv);
            }
        };
    }

    public ItemsAction<TaskScheduleDto> getItemsAction(TaskScheduleFilterComponent filterComponent) {
        return (query, orderList) -> {
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("id"));
            }
            TaskScheduleFilterDto filter = filterComponent.getTaskScheduleFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return taskScheduleService.findTaskDtoPage(filter);
        };
    }

    public List<TaskTypeDto> getAllTaskTypeList() {
        return taskTypeService.getAllAllowed();
    }
}

package cz.bbn.cerberus.taskfollowing;

import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingDto;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingFilterDto;
import cz.bbn.cerberus.taskfollowing.ui.component.TaskFollowingFilterComponent;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class TaskFollowingComponentOperation {

    private final TaskFollowingService taskFollowingService;
    private final AppEnv appEnv;
    private final UserService userService;

    public TaskFollowingComponentOperation(TaskFollowingService taskFollowingService,
                                           AppEnv appEnv, UserService userService) {
        this.taskFollowingService = taskFollowingService;
        this.appEnv = appEnv;
        this.userService = userService;
    }

    public ItemsAction<TaskFollowingDto> getItemsAction(TaskFollowingFilterComponent taskFollowingFilterComponent) {
        return (query, orderList) -> {
            TaskFollowingFilterDto filter = taskFollowingFilterComponent.getTaskFollowingFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return taskFollowingService.findTaskFollowingDtoPage(filter);
        };
    }

    public void saveTaskFollowing(Set<TaskFollowingDto> taskFollowingDtoSet) {
        taskFollowingService.saveTaskFollowing(taskFollowingDtoSet);
        SuccessNotification.showSavingSuccess(appEnv);
    }

    public Set<TaskFollowingDto> getFollowingDtoSet() {
        return taskFollowingService.findAllTaskFollowingDtoSet(SecurityUtils.getCurrentUserId());
    }

    public Set<TaskFollowingDto> getFollowingDtoAllSet(TaskFollowingFilterComponent taskFollowingFilterComponent) {
        List<UserDto> userDtoList = userService.findUserList().stream().filter(userDto ->
                userDto.getName()
                        .toLowerCase()
                        .contains(taskFollowingFilterComponent.getTaskFollowingFilterDto()
                                .getFollowingUserName()
                                .toLowerCase())
        ).toList();
        Set<TaskFollowingDto> taskFollowingDtoUnfollowedSet = new HashSet<>();
        userDtoList.forEach(userDto -> {
            TaskFollowingDto taskFollowingDto = new TaskFollowingDto();
            taskFollowingDto.setUserDto(SecurityUtils.getCurrentUserDto());
            taskFollowingDto.setFollowingUserDto(userDto);
            taskFollowingDtoUnfollowedSet.add(taskFollowingDto);
        });
        return taskFollowingDtoUnfollowedSet;
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                taskFollowingService.deleteTaskFollowing(SecurityUtils.getCurrentUserDto().getId(), Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}

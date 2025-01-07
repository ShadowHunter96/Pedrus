package cz.bbn.cerberus.taskfollowing.dto;

import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.util.Objects;
import java.util.Set;


@Getter
@Setter
public class TaskFollowingDto {

    private UserDto userDto;
    private UserDto followingUserDto;
    private Set<TaskDto> taskDtoList;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TaskFollowingDto that = (TaskFollowingDto) o;
        return Objects.equals(userDto.getId(), that.userDto.getId())
                && Objects.equals(followingUserDto.getId(), that.followingUserDto.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(userDto.getId(), followingUserDto.getId());
    }
}

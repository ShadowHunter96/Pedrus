package cz.bbn.cerberus.taskfollowing.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class TaskFollowingFilterDto {

    private String followingUserName;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}

package cz.bbn.cerberus.virtualserver.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class VirtualServerFilterDto {

    private Integer id;
    private String name;
    private String os;
    private Integer cpu;
    private Integer cores;
    private Integer ram;
    private String ip;
    private UserDto owner;
    private VirtualServerStatus status;
    private Boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}

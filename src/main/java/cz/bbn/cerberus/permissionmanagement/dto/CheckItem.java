package cz.bbn.cerberus.permissionmanagement.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CheckItem {

    private Long userId;
    private boolean checked;
    private Long fromUserId;

    public CheckItem(Long userId, boolean checked) {
        this.userId = userId;
        this.checked = checked;
    }
}

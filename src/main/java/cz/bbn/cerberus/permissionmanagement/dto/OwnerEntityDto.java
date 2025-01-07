package cz.bbn.cerberus.permissionmanagement.dto;


import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Objects;

@Getter
@Setter
public class OwnerEntityDto {

    private ObjectType objectType;
    private String entityId;
    private String entityName;
    private List<CheckItem> checkedItemList;
    private List<CheckItem> editPermissionList;
    private boolean isOwner;
    private boolean isChanged;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OwnerEntityDto that = (OwnerEntityDto) o;
        return entityId.equals(that.entityId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(entityId);
    }
}

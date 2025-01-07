package cz.bbn.cerberus.commons;

import cz.bbn.cerberus.permission.Permission;
import lombok.Getter;

@Getter
public class AppGridDataVariables {

    private final Permission editPermission;
    private final String dtoId;
    private final String dtoName;
    private final String route;
    private final Boolean hideDeleteButton;

    private String objectName = "";
    private Permission linkPermission = null;
    private Permission deletePermission = null;
    private Long connectionId = null;

    public AppGridDataVariables(Permission editPermission, Permission deletePermission,
                                String dtoId, String dtoName, String route, String objectName,
                                Boolean hideDeleteButton) {
        this.editPermission = editPermission;
        this.deletePermission = deletePermission;
        this.dtoId = dtoId;
        this.dtoName = dtoName;
        this.route = route;
        this.objectName = objectName;
        this.hideDeleteButton = hideDeleteButton;
    }

    public AppGridDataVariables(Permission editPermission, Permission deletePermission,
                                String dtoId, String dtoName, String route, Boolean hideDeleteButton) {
        this.editPermission = editPermission;
        this.deletePermission = deletePermission;
        this.dtoId = dtoId;
        this.dtoName = dtoName;
        this.route = route;
        this.hideDeleteButton = hideDeleteButton;
    }

    public AppGridDataVariables(Permission editPermission, Permission linkPermission,
                                String dtoId, Long connectionId, String dtoName, String route) {
        this.editPermission = editPermission;
        this.linkPermission = linkPermission;
        this.dtoId = dtoId;
        this.dtoName = dtoName;
        this.route = route;
        this.connectionId = connectionId;
        this.hideDeleteButton = true;
    }
}

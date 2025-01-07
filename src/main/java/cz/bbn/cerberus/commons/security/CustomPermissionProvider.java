package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.commons.DomainEnum;

import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class CustomPermissionProvider {

    protected CustomPermissionProvider() {
        CustomPermissionHandler.getInstance().addToCustomPermissionHandler(this);
    }

    protected abstract List<String> findAllId();

    protected abstract Set<DomainEnum> getDomainSet();

    protected abstract boolean showInCustomPermissions();

    protected abstract Map<String, Long> getOwnerMap();
}

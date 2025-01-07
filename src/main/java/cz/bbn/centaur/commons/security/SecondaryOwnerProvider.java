package cz.bbn.cerberus.commons.security;

import cz.bbn.cerberus.commons.DomainEnum;

import java.util.Set;

public abstract class SecondaryOwnerProvider {

    protected SecondaryOwnerProvider() {
        CustomPermissionHandler.getInstance().addToSecondaryOwnerProviderSet(this);
    }

    protected abstract Set<String> getUserEntityIdSet(Long userId);

    protected abstract Set<DomainEnum> getDomainSet();
}

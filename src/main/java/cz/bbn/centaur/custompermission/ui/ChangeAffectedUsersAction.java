package cz.bbn.cerberus.custompermission.ui;

import cz.bbn.cerberus.custompermission.dto.PermUserDto;

import java.util.Set;

public interface ChangeAffectedUsersAction {

    void changeAffectedUserSet(Set<PermUserDto> rightSet);
}

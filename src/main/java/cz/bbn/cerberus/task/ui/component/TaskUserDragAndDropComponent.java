package cz.bbn.cerberus.task.ui.component;

import cz.bbn.cerberus.commons.component.ui.AppDragAndDrop;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.Set;

public class TaskUserDragAndDropComponent extends AppDragAndDrop<UserDto> {

    protected TaskUserDragAndDropComponent(Set<UserDto> rightSet, Set<UserDto> leftSet, boolean canEdit) {
        super(rightSet, leftSet,
                "name",
                "Name",
                Transl.get("Users to invite"),
                Transl.get("Invited users"),
                UserDto.class, canEdit);
        setGridsHeight("15.5em");
        setGridsWidth("15em");
        setClassName("event-drag-and-drop");
    }

    @Override
    public String getSearchCompareVariable(UserDto object) {
        return object.getName();
    }
}

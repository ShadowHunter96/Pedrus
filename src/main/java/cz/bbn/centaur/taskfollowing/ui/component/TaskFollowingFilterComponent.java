package cz.bbn.cerberus.taskfollowing.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.taskfollowing.dto.TaskFollowingFilterDto;
import cz.bbn.cerberus.translation.Transl;

public class TaskFollowingFilterComponent extends FormLayout {

    private TextField followingUserName;

    private final Button search;

    public TaskFollowingFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    private void initComponent() {
        followingUserName = new TextField(Transl.get("Name"));
        this.add(followingUserName);
        this.add(search);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public TaskFollowingFilterDto getTaskFollowingFilterDto() {
        TaskFollowingFilterDto taskFilterDto = new TaskFollowingFilterDto();
        taskFilterDto.setFollowingUserName(followingUserName.getValue());
        return taskFilterDto;
    }
}

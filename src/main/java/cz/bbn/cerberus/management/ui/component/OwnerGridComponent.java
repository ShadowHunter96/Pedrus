package cz.bbn.cerberus.management.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.provider.Query;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.permissionmanagement.dto.OwnerEntityDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class OwnerGridComponent extends AppInfiniteGrid<OwnerEntityDto> {

    private final List<UserDto> userDtoSet;

    public OwnerGridComponent(AppEnv appEnv, Set<UserDto> userDtoSet, ItemsAction<OwnerEntityDto> itemsAction) {
        super(appEnv, itemsAction);
        this.userDtoSet = userDtoSet.stream().toList();
        this.getElement().getStyle().set("margin-top", "1.5em");
        initGrid();
        loadData();
    }

    private void initGrid() {
        addColumn(OwnerEntityDto::getEntityName).setHeader(Transl.get("Entity"));

        for (int i = 0; i < userDtoSet.size(); i++) {
            int finalI = i;
            addColumn(new ComponentRenderer<>(ownerEntityDto -> getColumn(ownerEntityDto, userDtoSet.get(finalI).getId(), finalI)))
                    .setHeader(getHeader(userDtoSet.get(finalI)));
        }
        Binder<OwnerEntityDto> binder = new Binder<>(OwnerEntityDto.class);
        getEditor().setBinder(binder);
    }

    private VerticalLayout getHeader(UserDto userDto){
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);
        verticalLayout.add(new Label(userDto.getName()));
        return verticalLayout;
    }

    private HorizontalLayout getColumn(OwnerEntityDto ownerEntityDto, Long userId, int position){
        HorizontalLayout layout = new HorizontalLayout();
        Checkbox checkboxOwner = new Checkbox(Transl.get("O"));
        layout.add(checkboxOwner);
        checkboxOwner.setValue(ownerEntityDto.getCheckedItemList().get(position).isChecked());
        checkboxOwner.addValueChangeListener(event -> {
            for (int i = 0; i < userDtoSet.size(); i++) {
                ownerEntityDto.getCheckedItemList().get(i).setChecked(i == position);
            }
            ownerEntityDto.setChanged(true);
            this.getEditor().editItem(ownerEntityDto);
            this.getEditor().save();
        });

        layout.add(new Label(" - "));
        Checkbox checkboxEditPermission = new Checkbox(Transl.get("E"));
        layout.add(checkboxEditPermission);
        checkboxEditPermission.setValue(ownerEntityDto.getEditPermissionList().get(position).isChecked());
        checkboxEditPermission.setReadOnly(true);
        layout.add(checkboxEditPermission);
        return layout;
    }

    public List<OwnerEntityDto> getDataForSave(){
        return getDataProvider().fetch(
                new Query<>()).collect(Collectors.toList()).stream().filter(OwnerEntityDto::isChanged).toList();
    }

}

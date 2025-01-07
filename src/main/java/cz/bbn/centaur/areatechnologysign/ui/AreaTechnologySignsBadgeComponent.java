package cz.bbn.cerberus.areatechnologysign.ui;

import com.vaadin.flow.component.orderedlayout.FlexLayout;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class AreaTechnologySignsBadgeComponent extends FlexLayout {

    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final ObjectType objectType;
    private final String objectId;

    public AreaTechnologySignsBadgeComponent(AreaTechnologyComponentOperation areaTechnologyComponentOperation, ObjectType objectType, String objectId) {
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.objectType = objectType;
        this.objectId = objectId;
        initComponent();
    }

    private void initComponent(){
        loadData();
        this.setFlexWrap(FlexWrap.WRAP);
    }

    public void loadData(){
        removeAll();
        List<AreaTechnologySignDto> areaTechnologySignDtoList = areaTechnologyComponentOperation.getListAction(objectType, objectId).getList(null);
        areaTechnologySignDtoList.forEach(areaTechnologySignDto -> {
            AreaTechnologySignBadgeComponent areaTechnologySignBadgeComponent = new AreaTechnologySignBadgeComponent(areaTechnologySignDto,
                    SecurityUtils.hasPermission(Permission.LABEL_EDIT) ?
                            areaTechnologyComponentOperation.getDeleteAction(this) : null);
            this.add(areaTechnologySignBadgeComponent);
        });
    }
}

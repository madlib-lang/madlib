#include "gc.h"
#include "uv.h"
#include "list.hpp"
#include "maybe.hpp"
#include "record.hpp"
#include "network.hpp"
#include <string.h>


#ifdef __cplusplus
extern "C" {
#endif

char *copyString(char *src) {
  size_t nameLength = strlen(src);
  char *dest = (char*) GC_MALLOC_ATOMIC(nameLength + 1);
  memcpy(dest, src, nameLength);
  dest[nameLength] = '\0';
  return dest;
}

madlib__list__Node_t *madlib__network__readNetworkInterfaces() {
    madlib__list__Node_t *result = madlib__list__empty();
    char buf[512];
    uv_interface_address_t *info;
    int count, i;

    uv_interface_addresses(&info, &count);
    i = count;

    while (i--) {
        madlib__record__Record_t *networkInterface = (madlib__record__Record_t*) GC_MALLOC(sizeof(madlib__record__Record_t));
        madlib__record__Field_t *ipv4Field = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
        ipv4Field->name = (char *) "ipv4";
        madlib__record__Field_t *ipv4MaskField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
        ipv4MaskField->name = (char *) "ipv4Mask";
        madlib__record__Field_t *ipv6Field = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
        ipv6Field->name = (char *) "ipv6";
        madlib__record__Field_t *ipv6MaskField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
        ipv6MaskField->name = (char *) "ipv6Mask";
        madlib__record__Field_t *isInternalField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
        isInternalField->name = (char *) "isInternal";
        madlib__record__Field_t *nameField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
        nameField->name = (char *) "name";

        networkInterface->fieldCount = 6;
        networkInterface->fields = (madlib__record__Field_t**) GC_MALLOC(sizeof(madlib__record__Field_t*) * 6);
        networkInterface->fields[0] = ipv4Field;
        networkInterface->fields[1] = ipv4MaskField;
        networkInterface->fields[2] = ipv6Field;
        networkInterface->fields[3] = ipv6MaskField;
        networkInterface->fields[4] = isInternalField;
        networkInterface->fields[5] = nameField;
        
        uv_interface_address_t interface_a = info[i];

        nameField->value = copyString(interface_a.name);
        isInternalField->value = (void*) interface_a.is_internal;

        madlib__maybe__Maybe_t *ipv4Maybe = (madlib__maybe__Maybe_t*) GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
        ipv4Maybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        ipv4Field->value = ipv4Maybe;
        madlib__maybe__Maybe_t *ipv4MaskMaybe = (madlib__maybe__Maybe_t*) GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
        ipv4MaskMaybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        ipv4MaskField->value = ipv4MaskMaybe;
        madlib__maybe__Maybe_t *ipv6Maybe = (madlib__maybe__Maybe_t*) GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
        ipv6Maybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        ipv6Field->value = ipv6Maybe;
        madlib__maybe__Maybe_t *ipv6MaskMaybe = (madlib__maybe__Maybe_t*) GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
        ipv6MaskMaybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        ipv6MaskField->value = ipv6MaskMaybe;

        if (interface_a.address.address4.sin_family == AF_INET) {
            uv_ip4_name(&interface_a.address.address4, buf, sizeof(buf));
            ipv4Maybe->index = madlib__maybe__Maybe_JUST_INDEX;
            ipv4Maybe->data = copyString(buf);

            uv_ip4_name(&interface_a.netmask.netmask4, buf, sizeof(buf));
            ipv4MaskMaybe->index = madlib__maybe__Maybe_JUST_INDEX;
            ipv4MaskMaybe->data = copyString(buf);
        }
        else if (interface_a.address.address4.sin_family == AF_INET6) {
            uv_ip6_name(&interface_a.address.address6, buf, sizeof(buf));
            ipv6Maybe->index = madlib__maybe__Maybe_JUST_INDEX;
            ipv6Maybe->data = copyString(buf);

            uv_ip6_name(&interface_a.netmask.netmask6, buf, sizeof(buf));
            ipv6MaskMaybe->index = madlib__maybe__Maybe_JUST_INDEX;
            ipv6MaskMaybe->data = copyString(buf);
        }

        result = madlib__list__internal__push(networkInterface, result);
    }

    uv_free_interface_addresses(info, count);

    return result;
}

#ifdef __cplusplus
}
#endif

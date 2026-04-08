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
  char *dest = (char*) malloc(nameLength + 1);
  memcpy(dest, src, nameLength);
  dest[nameLength] = '\0';
  return dest;
}

// NetworkInterface flat struct field indices (sorted alphabetically):
// [0]=ipv4, [1]=ipv4Mask, [2]=ipv6, [3]=ipv6Mask, [4]=isInternal, [5]=name
#define NETIF_IPV4         0
#define NETIF_IPV4MASK     1
#define NETIF_IPV6         2
#define NETIF_IPV6MASK     3
#define NETIF_IS_INTERNAL  4
#define NETIF_NAME         5
#define NETIF_FIELD_COUNT  6

madlib__list__Node_t *madlib__network__readNetworkInterfaces() {
    madlib__list__Node_t *result = madlib__list__empty();
    char buf[512];
    uv_interface_address_t *info;
    int count, i;

    uv_interface_addresses(&info, &count);
    i = count;

    while (i--) {
        void **networkInterface = (void**) malloc(sizeof(void*) * NETIF_FIELD_COUNT);

        uv_interface_address_t interface_a = info[i];

        madlib__maybe__Maybe_t *ipv4Maybe = (madlib__maybe__Maybe_t*) malloc(sizeof(madlib__maybe__Maybe_t));
        ipv4Maybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        networkInterface[NETIF_IPV4] = ipv4Maybe;

        madlib__maybe__Maybe_t *ipv4MaskMaybe = (madlib__maybe__Maybe_t*) malloc(sizeof(madlib__maybe__Maybe_t));
        ipv4MaskMaybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        networkInterface[NETIF_IPV4MASK] = ipv4MaskMaybe;

        madlib__maybe__Maybe_t *ipv6Maybe = (madlib__maybe__Maybe_t*) malloc(sizeof(madlib__maybe__Maybe_t));
        ipv6Maybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        networkInterface[NETIF_IPV6] = ipv6Maybe;

        madlib__maybe__Maybe_t *ipv6MaskMaybe = (madlib__maybe__Maybe_t*) malloc(sizeof(madlib__maybe__Maybe_t));
        ipv6MaskMaybe->index = madlib__maybe__Maybe_NOTHING_INDEX;
        networkInterface[NETIF_IPV6MASK] = ipv6MaskMaybe;

        networkInterface[NETIF_IS_INTERNAL] = (void*) interface_a.is_internal;
        networkInterface[NETIF_NAME] = copyString(interface_a.name);

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
